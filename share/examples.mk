ifndef TOP
$(error TOP is not set)
endif

ifndef PARENT
$(error PARENT is not set)
endif

SHELL := /usr/bin/env bash
DEPS  := core amazonka $(PARENT)

CABAL_SANDBOX_CONFIG := $(TOP)/cabal.sandbox.config

export CABAL_SANDBOX_CONFIG

build:
	cabal build

install: add-sources
	cabal install -j \
 --disable-documentation \
 --disable-library-coverage \
 --only-dependencies

add-sources: cabal.sandbox.config
	$(foreach dir,$(DEPS),cabal sandbox add-source $(TOP)/$(dir);)

cabal.sandbox.config:
	cabal sandbox init

clean:
	cabal clean
