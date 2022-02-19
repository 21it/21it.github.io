#!/bin/bash

set -e
set -x
shopt -s extglob

if [[ "$(git rev-parse --abbrev-ref HEAD)" != "source" ]]
then
  echo "Can release only from source branch, terminate"
  exit 1
fi

if [[ -n "$(git status -s -uall)" ]]
then
  echo "Uncommited changes detected, terminate"
  exit 1
fi

RELEASE_VERSION="$(git rev-parse --short HEAD)"
RELEASE_TMP_DIR="$(mktemp -d)"

stack build
stack exec -- site rebuild
cp -R ./_site/* $RELEASE_TMP_DIR/
git checkout master
rm -rf !(.git*)
cp -R $RELEASE_TMP_DIR/* .
git add -A
git commit -m "release $RELEASE_VERSION"
git push origin master
git checkout source
echo "release $RELEASE_VERSION succeeded"
