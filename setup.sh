#!/bin/bash

mkdir -p ~/.config/nvim/colors/

ln -s $(realpath ./init.lua) ~/.config/nvim/init.lua

ln -s $(realpath ./usgc.lua) ~/.config/nvim/colors/usgc.lua
