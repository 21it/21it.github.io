#!/bin/sh

set -e

THIS_DIR="$(dirname "$(realpath "$0")")"

stack build
stack exec site clean
stack exec -- site rebuild
(
  cd "$THIS_DIR/docs"
  htmldoc --webpage -f cv.pdf ./formal.html
)
