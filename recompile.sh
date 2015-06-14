#!/bin/bash -ue

# Hacky script.

# Place where there is a cabal sandbox for xmonad, for now
cd ~/src/xmonad/xmonad  

cabal exec -- xmonad --recompile
cabal exec -- xmonad --restart
