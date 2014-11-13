ifndef TOP
$(error TOP is not set)
endif

SHELL := /usr/bin/env bash
CABAL_SANDBOX_CONFIG := $(TOP)/cabal.sandbox.config

export CABAL_SANDBOX_CONFIG

build:
	cabal build -j

install: cabal.sandbox.config
	cabal install -j \
 --disable-documentation \
 --disable-library-coverage \
 --only-dependencies

cabal.sandbox.config:
	cabal sandbox add-source $(TOP)/core

clean:
	cabal clean

doc:
	cabal haddock
