ifndef TOP
$(error TOP is not set)
endif

SHELL   := /usr/bin/env bash
NAME    ?= $(notdir $(CURDIR:a/%=%))
VERSION ?= $(shell sed -n 's/^version: *\(.*\)$$/\1/p' $(NAME).cabal)

CABAL_SANDBOX_CONFIG := $(TOP)/cabal.sandbox.config

export CABAL_SANDBOX_CONFIG

build:
	cabal build -j

install: add-sources
	cabal install -j \
 --disable-documentation \
 --disable-library-coverage \
 --only-dependencies \
 --force-reinstalls # https://github.com/haskell/cabal/issues/2101

add-sources:
	cabal sandbox add-source $(TOP)/core

clean:
	cabal clean

sdist:
	cabal sdist

upload:
	cabal upload dist/$(NAME)-$(VERSION).tar.gz

doc:
	cabal haddock
