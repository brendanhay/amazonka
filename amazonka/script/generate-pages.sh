#!/usr/bin/env bash

set -e

doc=$HOME/Projects/aws-doc

standalone-haddock \
 -o $doc \
 --package-db $HOME/Projects/aws/amazonka/.cabal-sandbox/x86_64-osx-ghc-7.8.2-packages.conf.d \
 $HOME/Projects/aws/amazonka

cd $doc

git checkout gh-pages || true
git add .
git commit -am "Updating"
git push origin gh-pages
