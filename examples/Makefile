SHELL := /usr/bin/env bash
DEPS  := core amazonka amazonka-sqs

build:
	cabal build

install: add-sources
	cabal install -j \
 --disable-documentation \
 --disable-library-coverage \
 --only-dependencies

add-sources: cabal.sandbox.config
	$(foreach dir,$(DEPS),cabal sandbox add-source ../$(dir);)

cabal.sandbox.config:
	cabal sandbox init

clean:
	rm -rf cabal.sandbox.config .cabal-sandbox
	cabal clean
