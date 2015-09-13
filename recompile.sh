#!/bin/bash -ue

prefix=
export PATH=/usr/bin:$PATH

${prefix} xmonad --recompile
${prefix} xmonad --restart
