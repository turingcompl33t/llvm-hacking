#!/usr/bin/zsh
# llvm.sh
# Configure environment for working with LLVM install.
#
# Not executable, just source this script to enable LLVM tools in the shell.

# assumes llvm 10 installed (by apt)
export PATH=$PATH:/usr/lib/llvm-10/bin