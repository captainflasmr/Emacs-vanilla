#!/usr/bin/env bash
#
# setup-vm-keyboard.sh — Configure keyboard remapping and sticky keys
#                         inside a GNOME Wayland VM guest
#
# This replicates the host sway keyboard configuration from
# keymap_with_sticky_modifiers.xkb for use inside QEMU VMs where
# host XKB settings don't pass through SPICE/VNC.
#
# Remappings:
#   - Caps Lock → Control
#   - Right Alt → Control
#   - Sticky modifier keys (accessibility)
#   - GB keyboard layout
#
# Usage:
#   ssh into the VM and run this script, or:
#     qvm scp <name> setup-vm-keyboard.sh :/home/$USER/
#     qvm ssh <name> -- bash setup-vm-keyboard.sh
#
# After running, log out and back in for changes to take effect.

set -euo pipefail

echo "Configuring keyboard layout: GB"
gsettings set org.gnome.desktop.input-sources sources "[('xkb', 'gb')]"

echo "Configuring key remappings: Caps Lock → Ctrl, Right Alt → Ctrl"
gsettings set org.gnome.desktop.input-sources xkb-options "['ctrl:nocaps','ctrl:ralt_rctrl']"

echo "Enabling sticky keys"
gsettings set org.gnome.desktop.a11y.keyboard stickykeys-enable true

echo ""
echo "Done. Log out and back in for changes to take effect."
