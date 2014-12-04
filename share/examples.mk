ifndef TOP
$(error TOP is not set)
endif

ifndef PARENT
$(error PARENT is not set)
endif

SHELL := /usr/bin/env bash
DEPS  := core amazonka $(PARENT)

build:
	cabal build

install: add-sources
	cabal install -j \
 --disable-documentation \
 --disable-library-coverage \
 --only-dependencies \
 --force-reinstalls

add-sources: cabal.sandbox.config
	$(foreach dir,$(DEPS),cabal sandbox add-source $(realpath $(TOP)/$(dir));)

cabal.sandbox.config:
	cabal sandbox init

clean:
	cabal clean
	rm -rf cabal.sandbox.config .cabal-sandbox

repl:
	cabal repl
