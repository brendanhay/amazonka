#!/usr/bin/env bash

exec standalone-haddock \
 -o $HOME/Projects/aws-doc \
 --package-db $HOME/Projects/aws/.cabal-sandbox/x86_64-osx-ghc-7.8.2-packages.conf.d \
 amazonka-types
