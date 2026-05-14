#!/bin/bash

mkdir -p ~/.config/nvim/colors/

ln -s $(realpath ./init.lua) ~/.config/nvim/init.lua

ln -s $(realpath ./usgc.lua) ~/.config/nvim/colors/usgc.lua

# Configure emacs

emacs_dir="$HOME"/.emacs.d

if [ ! -d $emacs_dir ]; then
    mkdir $emacs_dir
fi

for config_file in $( ls ./emacs/*.el ); do
    file=$(realpath $config_file)
    base=$(basename $file)
    ln -s "$file" "$emacs_dir"/"$base"
done
