#!/bin/bash

# Abort on any unchecked failure. Without this, a failing `make` silently
# flows into "installed to..." and the outer loop, masking real build errors.
set -eo pipefail

# Directory for Emacs builds
BUILD_ROOT="$HOME/emacs-builds"
INSTALL_ROOT="$HOME/emacs-versions"

# Build dependencies for different distributions.
# NOTE: ARCH_BUILD_DEPS covers both classic X11/GTK3 builds AND Wayland-native
# PGTK builds (--with-pgtk). PGTK needs gtk3 + wayland plus the modern feature
# libs (jansson, tree-sitter, libgccjit for native-comp, sqlite, webp, lcms2,
# libotf, m17n-lib, libsystemd) and the autotools chain for git checkouts.
ARCH_BUILD_DEPS="base-devel gtk3 wayland libxpm libjpeg-turbo libpng libtiff giflib libxml2 gnutls librsvg cairo harfbuzz jansson tree-sitter libgccjit sqlite libsystemd webp lcms2 libotf m17n-lib texinfo autoconf automake git pkgconf"
# SLES/SLED split: CORE must exist on vanilla SLES 15 SP4-SP6 base modules
# (this is the pre-36111a0 set plus autoconf/git, both verified present).
# OPTIONAL holds modern feature libs that are missing without PackageHub /
# Backports (or have different names than Fedora/Debian). They are installed
# best-effort and configure degrades gracefully when they are absent, so a
# missing optional package must NEVER abort the build (zypper exits 104).
SLES_CORE_DEPS="gcc gcc-c++ make automake autoconf texinfo gtk3-devel libXpm-devel libjpeg8-devel libpng16-devel libtiff-devel giflib-devel libxml2-devel gnutls-devel cairo-devel harfbuzz-devel librsvg-devel git"
# NOTE on names (openSUSE/SLES convention, not Fedora/Debian):
#   jansson -> libjansson-devel (not jansson-devel)
#   lcms2   -> lcms2-devel
#   tree-sitter -> libtree-sitter-devel (Leap 15.5+ oss; absent on plain SLES)
#   native-comp -> libgccjit-devel (absent on SLES with gcc7; needs gcc>=10)
SLES_OPTIONAL_DEPS="libjansson-devel sqlite3-devel libwebp-devel lcms2-devel libtree-sitter-devel libgccjit-devel"
# Kept for backwards compat (scripts grepping for it); install logic below
# uses CORE (fatal) + OPTIONAL (best-effort) separately.
SLES_BUILD_DEPS="$SLES_CORE_DEPS $SLES_OPTIONAL_DEPS"

# Upstream git for bleeding-edge (Wayland-native PGTK) builds.
EMACS_GIT_URL="https://git.savannah.gnu.org/git/emacs.git"
EMACS_GIT_SRC="$BUILD_ROOT/emacs-git"

# 27.2 2021-03-25
# 28.2 2022-09-12
# 29.4 2024-06-22
# 30.1 2025-02-23 / 30.2 2025-08 (current pacman emacs-wayland 30.2-3 is PGTK)
DEFAULT_VERSIONS=(
    "emacs-27.2"
    "emacs-28.2"
    "emacs-29.4"
    "emacs-30.1"
    "emacs-31.1"
)

# If a single version arg is given (e.g. "27.2", "emacs-27.2", "master",
# "git", "pgtk", "emacs-master") build just that one; otherwise build the
# default set above. "master" means bleeding-edge from Savannah git with
# Wayland-native PGTK + native-comp + tree-sitter.
if [[ $# -ge 1 && -n "$1" ]]; then
    v="$1"
    case "$v" in
        master|git|pgtk|emacs-master|emacs-git|emacs-pgtk)
            VERSIONS=("emacs-master")
            ;;
        *)
            [[ "$v" == emacs-* ]] || v="emacs-$v"
            VERSIONS=("$v")
            ;;
    esac
else
    VERSIONS=("${DEFAULT_VERSIONS[@]}")
fi

# Detect OS
detect_os() {
    if [ -f /etc/os-release ]; then
        # shellcheck disable=SC1091
        . /etc/os-release
        # Combine fields so matching works for SLES ("SLES"), SLED ("SLED"),
        # openSUSE ("openSUSE Leap") and derivatives regardless of which
        # field the distro fills in (NAME vs ID vs PRETTY_NAME).
        OS="${NAME:-} ${ID:-} ${ID_LIKE:-} ${PRETTY_NAME:-}"
    else
        OS=$(uname -s)
    fi
}

function prepare_environment() {
    echo "Creating build directories..."
    mkdir -p "$BUILD_ROOT"
    mkdir -p "$INSTALL_ROOT"
    
    detect_os
    echo "Detected OS: $OS"
    
    case "$OS" in
        *"SLED"*|*"SLES"*|*"SUSE"*|*"sles"*|*"sled"*|*"opensuse"*|*"leap"*)
            echo "Installing build dependencies for SUSE SLES/SLED..."
            sudo zypper refresh || echo "WARN: 'zypper refresh' failed, continuing anyway..."
            # The old line `zypper install -y pattern-devel-base-devel` is not
            # valid zypper syntax (patterns need `-t pattern`) and broke SLES
            # SP4 builds. The correct pattern is `devel_basis`, but it is only
            # best-effort: the explicit toolchain packages below already cover
            # a minimal build, so this must never abort the script.
            sudo zypper install -y -t pattern devel_basis \
                || echo "WARN: pattern 'devel_basis' not available, continuing with explicit packages..."
            # Core deps: fatal on failure (without these no build can work).
            # shellcheck disable=SC2086
            sudo zypper install -y $SLES_CORE_DEPS \
                || { echo "ERROR: core SLES build dependencies failed to install" >&2; exit 1; }
            # Optional modern feature libs (json/sqlite/webp/lcms2/tree-sitter/
            # native-comp). Missing on plain SLES SP4-SP6 without PackageHub /
            # Backports, so install each best-effort under `set -e`.
            for pkg in $SLES_OPTIONAL_DEPS; do
                sudo zypper install -y "$pkg" \
                    || echo "WARN: optional package '$pkg' not available, continuing without it..."
            done
            # Legacy/alternate names some SLES snapshots provide instead.
            for pkg in jansson-devel tree-sitter-devel liblcms2-devel; do
                if ! rpm -q "$pkg" >/dev/null 2>&1; then
                    sudo zypper install -y "$pkg" >/dev/null 2>&1 \
                        && echo "Installed alternate optional package '$pkg'." \
                            || true
                fi
            done
            ;;
        *"Garuda"*)
            echo "Installing build dependencies for Arch Linux..."
            sudo pacman -Syu --needed --noconfirm $ARCH_BUILD_DEPS
            
            # Check if we have yay for AUR access (optional)
            if ! command -v yay &> /dev/null; then
                echo "Installing yay (AUR helper)..."
                cd /tmp
                git clone https://aur.archlinux.org/yay.git
                cd yay
                makepkg -si --noconfirm
            fi
            ;;
        *)
            echo "Unsupported OS detected: $OS"
            echo "Please install build dependencies manually and continue."
            read -p "Press Enter to continue or Ctrl+C to abort..."
            ;;
    esac
}

function build_emacs() {
    local version=$1
    local build_dir="$BUILD_ROOT/$version"
    local install_dir="$INSTALL_ROOT/$version"
    
    echo "Building $version..."

    cd "$BUILD_ROOT"

    # Source resolution: prefer a pre-staged tarball under $SOURCES_DIR (the
    # offline-packages/sources/ dir populated by fetch-source.sh), then fall
    # back to whatever is already in $BUILD_ROOT, else download.
    local src=""
    for cand in \
        "${SOURCES_DIR:-}/$version.tar.xz" \
            "${SOURCES_DIR:-}/$version.tar.gz" \
            "$BUILD_ROOT/$version.tar.xz" \
            "$BUILD_ROOT/$version.tar.gz"; do
        [[ -z "$cand" || "$cand" == "/$version."* ]] && continue
        if [[ -f "$cand" ]]; then
            src="$cand"
            break
        fi
    done

    if [[ -z "$src" ]]; then
        src="$BUILD_ROOT/$version.tar.xz"
        echo "Downloading $version.tar.xz from ftp.gnu.org..."
        wget -O "$src" "https://ftp.gnu.org/gnu/emacs/$version.tar.xz"
    else
        echo "Using local source: $src"
    fi

    # Clean previous build if exists
    rm -rf "$build_dir"
    tar -xf "$src"   # auto-detects .tar.gz / .tar.xz
    
    # Configure and build
    cd "$version"
    
    # Different configure flags for different versions
    if [[ "$version" == "emacs-24.5" || "$version" == "emacs-25.3" ]]; then
        # Older versions use GTK2
        ./configure \
            --prefix="$install_dir" \
            --with-x-toolkit=gtk2 \
            --with-xpm \
            --with-jpeg \
            --with-png \
            --with-gif \
            --with-tiff \
            --with-gnutls \
            --with-xml2 \
            --with-rsvg
    else
        # Newer versions use GTK3 (X11). Modern feature flags are appended
        # where supported; very old releases ignore unknown --with-* options
        # only if configure tolerates them, so keep the X11 set conservative
        # for <29 and full-featured for >=29.
        # On SLES SP4-SP6 without PackageHub, libjansson/libgccjit/tree-sitter
        # are absent (and gcc7 cannot do native-comp), so the full configure
        # can fail. Fall back to the conservative X11 set instead of aborting
        # -- that is exactly how Emacs 31.1 was verified to build on SLES SP4.
        if [[ "$version" == emacs-29* || "$version" == emacs-30* || "$version" == emacs-31* ]]; then
            if ! ./configure \
                 --prefix="$install_dir" \
                 --with-x-toolkit=gtk3 \
                 --with-xpm \
                 --with-jpeg \
                 --with-png \
                 --with-gif \
                 --with-tiff \
                 --with-gnutls \
                 --with-xml2 \
                 --with-cairo \
                 --with-harfbuzz \
                 --with-rsvg \
                 --with-json \
                 --with-sqlite3 \
                 --with-webp \
                 --with-lcms2 \
                 --with-modules \
                 --with-tree-sitter \
                 --with-native-compilation=aot; then
                echo "WARN: full-featured configure failed for $version."
                echo "WARN: retrying with conservative flags (no json/sqlite/webp/lcms2/tree-sitter/native-comp)..."
                ./configure \
                    --prefix="$install_dir" \
                    --with-x-toolkit=gtk3 \
                    --with-xpm \
                    --with-jpeg \
                    --with-png \
                    --with-gif \
                    --with-tiff \
                    --with-gnutls \
                    --with-xml2 \
                    --with-cairo \
                    --with-harfbuzz \
                    --with-rsvg
            fi
        else
            ./configure \
                --prefix="$install_dir" \
                --with-x-toolkit=gtk3 \
                --with-xpm \
                --with-jpeg \
                --with-png \
                --with-gif \
                --with-tiff \
                --with-gnutls \
                --with-xml2 \
                --with-cairo \
                --with-harfbuzz \
                --with-rsvg
        fi
    fi
    
    # Use all available cores for compilation. set -e aborts on failure; the
    # explicit messages make the failing phase obvious in long build logs.
    make -j"$(nproc)" || { echo "ERROR: make failed for $version" >&2; exit 1; }
    make install     || { echo "ERROR: make install failed for $version" >&2; exit 1; }

    echo "$version installed to $install_dir"
}

function build_emacs_master() {
    # Bleeding-edge Wayland-native build from Savannah master.
    # Mirrors Arch extra/emacs-wayland flags: PGTK + cairo + harfbuzz +
    # libsystemd + modules + native-comp (aot) + tree-sitter, plus json /
    # sqlite / webp / lcms2 / rsvg autodetected via pkg-config.
    local version="emacs-master"
    local install_dir="$INSTALL_ROOT/$version"

    echo "Building $version (git master, PGTK/Wayland) ..."

    mkdir -p "$BUILD_ROOT"
    mkdir -p "$INSTALL_ROOT"

    if [[ -d "$EMACS_GIT_SRC/.git" ]]; then
        echo "Updating existing checkout at $EMACS_GIT_SRC ..."
        git -C "$EMACS_GIT_SRC" fetch --all --prune
        git -C "$EMACS_GIT_SRC" checkout master
        git -C "$EMACS_GIT_SRC" pull --ff-only
    else
        echo "Cloning $EMACS_GIT_URL -> $EMACS_GIT_SRC ..."
        rm -rf "$EMACS_GIT_SRC"
        git clone "$EMACS_GIT_URL" "$EMACS_GIT_SRC"
    fi

    cd "$EMACS_GIT_SRC"

    echo "Running autogen.sh ..."
    ./autogen.sh

    # NOTE: no --with-json flag on master (>=31): libjansson support is
    # autodetected and the option was removed from configure.
    # PGTK + native-comp + tree-sitter need a modern toolchain (gcc>=10,
    # libgccjit, tree-sitter). On SLES SP4-SP6 the full configure fails, so
    # degrade gracefully: full PGTK -> minimal PGTK -> conservative X11/GTK3
    # (the X11 fallback is what builds on SLES SP4).
    echo "Configuring $version with PGTK ..."
    if ! ./configure \
         --prefix="$install_dir" \
         --with-pgtk \
         --with-cairo \
         --with-harfbuzz \
         --with-libsystemd \
         --with-modules \
         --with-native-compilation=aot \
         --with-tree-sitter \
         --with-sqlite3 \
         --with-webp \
         --with-lcms2 \
         --with-rsvg \
         --with-xml2 \
         --with-gnutls \
         --with-xpm \
         --with-jpeg \
         --with-png \
         --with-gif \
         --with-tiff; then
        echo "WARN: full PGTK configure failed; retrying minimal PGTK (no systemd/native-comp/tree-sitter/sqlite/webp/lcms2)..."
        if ! ./configure \
             --prefix="$install_dir" \
             --with-pgtk \
             --with-cairo \
             --with-harfbuzz \
             --with-modules \
             --with-rsvg \
             --with-xml2 \
             --with-gnutls \
             --with-xpm \
             --with-jpeg \
             --with-png \
             --with-gif \
             --with-tiff; then
            echo "WARN: minimal PGTK configure failed; falling back to X11/GTK3..."
            ./configure \
                --prefix="$install_dir" \
                --with-x-toolkit=gtk3 \
                --with-xpm \
                --with-jpeg \
                --with-png \
                --with-gif \
                --with-tiff \
                --with-gnutls \
                --with-xml2 \
                --with-cairo \
                --with-harfbuzz \
                --with-rsvg
        fi
    fi

    # Use all available cores for compilation. set -e aborts on failure; the
    # explicit messages make the failing phase obvious in long build logs.
    make -j"$(nproc)" || { echo "ERROR: make failed for $version" >&2; exit 1; }
    make install     || { echo "ERROR: make install failed for $version" >&2; exit 1; }

    echo "$version installed to $install_dir"
    "$install_dir/bin/emacs" --version | head -n 3
}

function create_pkgbuild() {
    # Only create PKGBUILD for Arch Linux
    if [[ "$OS" != *"Arch Linux"* ]]; then
        echo "PKGBUILD creation is only supported on Arch Linux"
        return 1
    fi

    local version=$1
    local version_num=${version#emacs-}
    
    echo "Creating PKGBUILD for $version..."
    mkdir -p "$BUILD_ROOT/pkgbuilds/$version"
    cd "$BUILD_ROOT/pkgbuilds/$version"
    
    cat > PKGBUILD << EOF
# Maintainer: Your Name <your.email@example.com>
pkgname=$version
pkgver=$version_num
pkgrel=1
pkgdesc="GNU Emacs version $version_num"
arch=('x86_64')
url="https://www.gnu.org/software/emacs/"
license=('GPL3')
depends=('gtk3' 'libxpm' 'libjpeg-turbo' 'libpng' 'giflib' 'libtiff' 'libxml2' 'gnutls')
makedepends=('base-devel')
provides=("emacs-$version_num")
conflicts=("emacs")
source=("https://ftp.gnu.org/gnu/emacs/emacs-\$pkgver.tar.gz")
sha256sums=('SKIP')

build() {
    cd "\$srcdir/emacs-\$pkgver"
    ./configure \\
        --prefix=/usr \\
        --sysconfdir=/etc \\
        --libexecdir=/usr/lib \\
        --localstatedir=/var \\
        --with-x-toolkit=gtk3 \\
        --with-xpm \\
        --with-jpeg \\
        --with-png \\
        --with-gif \\
        --with-tiff \\
        --with-gnutls \\
        --with-xml2
    make
        }

package() {
    cd "\$srcdir/emacs-\$pkgver"
    make DESTDIR="\$pkgdir" install
          }
EOF
}

# Main execution
# BUILD_METHOD env var skips the prompt (1=direct, 2=makepkg). Useful when
# called non-interactively (e.g. from create-install.sh).
if [[ -n "${BUILD_METHOD:-}" ]]; then
    build_method="$BUILD_METHOD"
else
    echo "This script provides two methods to build Emacs:"
    echo "1. Direct compilation (traditional)"
    echo "2. Using makepkg (Arch Linux only)"
    read -p "Which method do you prefer? (1/2): " build_method
fi

case $build_method in
    1)
        # SKIP_PREPARE=1 skips pacman/zypper installs (deps already present).
        if [[ -z "${SKIP_PREPARE:-}" ]]; then
            prepare_environment
        else
            echo "Skipping prepare_environment (SKIP_PREPARE set)..."
            mkdir -p "$BUILD_ROOT" "$INSTALL_ROOT"
        fi
        for version in "${VERSIONS[@]}"; do
            if [[ "$version" == "emacs-master" ]]; then
                build_emacs_master
            else
                build_emacs "$version"
            fi
        done

        # Create convenience symlinks
        mkdir -p "$HOME/bin"
        echo "Creating version-specific symlinks..."
        for version in "${VERSIONS[@]}"; do
            ln -sf "$INSTALL_ROOT/$version/bin/emacs" "$HOME/bin/emacs-${version#emacs-}"
        done
        # If master was built, point bare ~/bin/emacs at it so it shadows
        # /usr/bin/emacs (~/bin precedes /usr/bin in PATH). Back up any
        # existing ~/bin/emacs symlink first. Pass MAKE_DEFAULT=no to skip.
        if [[ " ${VERSIONS[*]} " == *" emacs-master "* && "${MAKE_DEFAULT:-yes}" != "no" ]]; then
            if [[ -e "$HOME/bin/emacs" && ! -L "$HOME/bin/emacs" ]]; then
                echo "NOTE: $HOME/bin/emacs is a regular file; leaving it alone."
            else
                echo "Pointing $HOME/bin/emacs -> emacs-master (backs up old link)..."
                [[ -L "$HOME/bin/emacs" ]] && cp -P "$HOME/bin/emacs" "$HOME/bin/emacs.prev-link"
                ln -sf "$INSTALL_ROOT/emacs-master/bin/emacs" "$HOME/bin/emacs"
            fi
            echo "Master is now the default 'emacs' in PATH. Verify with:"
            echo "  hash -r; emacs --version; emacs --batch --eval '(message \"%s\" system-configuration-features)'"
            echo "To revert to pacman emacs-wayland: rm ~/bin/emacs (restores /usr/bin/emacs 30.2)."
        fi
        ;;
    
    2)
        detect_os
        if [[ "$OS" != *"Arch Linux"* ]]; then
            echo "makepkg method is only supported on Arch Linux"
            exit 1
        fi
        prepare_environment
        for version in "${VERSIONS[@]}"; do
            create_pkgbuild "$version"
            echo "PKGBUILD created for $version"
            echo "To build, cd to $BUILD_ROOT/pkgbuilds/$version and run 'makepkg -si'"
        done
        ;;
    
    *)
        echo "Invalid option selected"
        exit 1
        ;;
esac

echo "Build complete. You can run specific versions using:"
for version in "${VERSIONS[@]}"; do
    echo "emacs-${version#emacs-}"
done
echo ""
echo "Usage notes:"
echo "  ./build-emacs-versions.sh master   # bleeding-edge PGTK/Wayland -> ~/emacs-versions/emacs-master"
echo "  ./build-emacs-versions.sh 30.2     # stable tarball (X11/GTK3)"
echo "  MAKE_DEFAULT=no ./build-emacs-versions.sh master  # skip ~/bin/emacs repoint"
