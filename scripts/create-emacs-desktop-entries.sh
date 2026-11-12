#!/usr/bin/env bash
# create-emacs-desktop-entries.sh — make installed Emacs builds visible in
# rofi's drun mode by writing one .desktop entry per build under
# ~/emacs-versions (or $EMACS_INSTALL_ROOT).
#
# The entries are safe to regenerate: each build's file is overwritten with
# the same name (emacs-<version>.desktop).  Run this after
# build-emacs-versions.sh, or let that script call it automatically.

set -euo pipefail

INSTALL_ROOT="${EMACS_INSTALL_ROOT:-$HOME/emacs-versions}"
APP_DIR="${XDG_DATA_HOME:-$HOME/.local/share}/applications"
ICON="${EMACS_ICON:-emacs}"

mkdir -p "$APP_DIR"

write_entry() {
    local version="$1"
    local version_num="${version#emacs-}"
    local emacs_bin="$INSTALL_ROOT/$version/bin/emacs"
    local desktop_file="$APP_DIR/$version.desktop"
    local display_name
    local comment

    [[ -x "$emacs_bin" ]] || return 0

    if [[ "$version" == "emacs-master" ]]; then
        display_name="Emacs (Master)"
        comment="GNU Emacs bleeding-edge master build (PGTK/Wayland)"
    else
        display_name="Emacs $version_num"
        comment="GNU Emacs $version_num (PGTK/Wayland)"
    fi

    cat > "$desktop_file" <<EOF
[Desktop Entry]
Name=$display_name
GenericName=Text Editor
Comment=$comment
TryExec=$emacs_bin
Exec=$emacs_bin %F
Icon=$ICON
Type=Application
Terminal=false
Categories=Development;TextEditor;
StartupNotify=true
StartupWMClass=Emacs
Keywords=Text;Editor;Emacs;$version_num;
MimeType=text/english;text/plain;text/x-makefile;text/x-c++hdr;text/x-c++src;text/x-chdr;text/x-csrc;text/x-java;text/x-moc;text/x-pascal;text/x-tcl;text/x-tex;application/x-shellscript;text/x-c;text/x-c++;
Actions=new-window;new-instance;

[Desktop Action new-window]
Name=New Window
Exec=$emacs_bin %F

[Desktop Action new-instance]
Name=New Instance (--debug-init)
Exec=$emacs_bin --debug-init %F
EOF

    chmod 644 "$desktop_file"
    echo "Created/updated $desktop_file"
}

shopt -s nullglob
for version_dir in "$INSTALL_ROOT"/emacs-*; do
    write_entry "${version_dir##*/}"
done
shopt -u nullglob

if command -v update-desktop-database >/dev/null 2>&1; then
    update-desktop-database "$APP_DIR" >/dev/null 2>&1 || true
fi

echo "Emacs desktop entries are in $APP_DIR"
