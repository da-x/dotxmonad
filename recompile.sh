#!/bin/bash -ue

cd ~/src/xmonad/xmonad
cabal exec -- xmonad --recompile
cabal exec -- xmonad --restart
