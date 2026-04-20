#!/bin/bash

# Abort on any unchecked failure. Without this, a failing `make` silently
# flows into "installed to..." and the outer loop, masking real build errors.
set -eo pipefail

# Directory for Emacs builds
BUILD_ROOT="$HOME/emacs-builds"
INSTALL_ROOT="$HOME/emacs-versions"

# Build dependencies for different distributions
ARCH_BUILD_DEPS="base-devel gtk2 gtk3 libxpm libjpeg-turbo libpng libtiff giflib libxml2 gnutls librsvg"
SLES_BUILD_DEPS="gcc gcc-c++ make automake texinfo gtk2-devel gtk3-devel libXpm-devel libjpeg8-devel libpng16-devel libtiff-devel giflib-devel libxml2-devel gnutls-devel cairo-devel harfbuzz-devel librsvg-devel"

# 27.2 2021-03-25
# 28.2 2022-09-12
# 29.4 2024-06-22
DEFAULT_VERSIONS=(
    "emacs-27.2"
    "emacs-28.2"
    "emacs-29.4"
    "emacs-30.1"
)

# If a single version arg is given (e.g. "27.2" or "emacs-27.2") build just
# that one; otherwise build the default set above.
if [[ $# -ge 1 && -n "$1" ]]; then
    v="$1"
    [[ "$v" == emacs-* ]] || v="emacs-$v"
    VERSIONS=("$v")
else
    VERSIONS=("${DEFAULT_VERSIONS[@]}")
fi

# Detect OS
detect_os() {
    if [ -f /etc/os-release ]; then
        . /etc/os-release
        OS=$NAME
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
        *"SLED"*|"SLES"*|*"SUSE"*)
            echo "Installing build dependencies for SUSE SLES..."
            sudo zypper refresh
            sudo zypper install -y pattern-devel-base-devel
            sudo zypper install -y $SLES_BUILD_DEPS
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
        # Newer versions use GTK3
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
    
    # Use all available cores for compilation. set -e aborts on failure; the
    # explicit messages make the failing phase obvious in long build logs.
    make -j"$(nproc)" || { echo "ERROR: make failed for $version" >&2; exit 1; }
    make install     || { echo "ERROR: make install failed for $version" >&2; exit 1; }

    echo "$version installed to $install_dir"
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
        prepare_environment
        for version in "${VERSIONS[@]}"; do
            build_emacs "$version"
        done
        
        # Create convenience symlinks
        mkdir -p "$HOME/bin"
        echo "Creating version-specific symlinks..."
        for version in "${VERSIONS[@]}"; do
            ln -sf "$INSTALL_ROOT/$version/bin/emacs" "$HOME/bin/emacs-${version#emacs-}"
        done
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
