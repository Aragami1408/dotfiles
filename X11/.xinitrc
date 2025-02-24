#!/bin/sh

xbacklight -set 10 &
xset r rate 200 25 &
~/.screenlayout/vm_resolution.sh &
~/.config/dwm/bar/bar.sh &
(sleep 1 ; feh --bg-fill ~/Pictures/frieren-desktop.jpg) &
exec dwm
