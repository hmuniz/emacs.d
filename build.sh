#!/bin/bash

git clone git://git.sv.gnu.org/emacs.git

sudo apt install build-essential libgtk-3-dev \
	libgnutls28-dev libtiff5-dev libgif-dev \
	libjpeg-dev libpng-dev libxpm-dev \
	libncurses-dev texinfo libjansson4 libjansson-dev \
	libgccjit0 libgccjit-10-dev gcc-10 g++-10

cd emacs

export CC=/usr/bin/gcc-10 CXX=/usr/bin/gcc-10

./configure --with-native-compilation --with-json --with-pgtk --with-mailutils       --with-compress-install --with-threads --with-included-regex --with-zlib       --with-jpeg --with-png --with-imagemagick --with-tiff --with-xpm

./configure --with-native-compilation --with-json --with-pgtk --with-mailutils
