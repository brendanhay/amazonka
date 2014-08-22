#!/usr/bin/env bash

set -e

doc=$HOME/Projects/aws-doc
libs=$HOME/Projects/aws/amazonka*

standalone-haddock \
 -o $doc \
 --package-db $HOME/Projects/aws/amazonka/.cabal-sandbox/x86_64-osx-ghc-7.8.2-packages.conf.d \
 $libs

cd $doc

git checkout gh-pages || true
git add .
git commit -am "Updating"
git push origin gh-pages
