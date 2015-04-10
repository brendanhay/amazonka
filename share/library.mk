ifndef TOP
$(error variable TOP is not set)
endif

SHELL   := /usr/bin/env bash
NAME    ?= $(notdir $(CURDIR:a/%=%))
VERSION ?= $(shell sed -n 's/^version: *\(.*\)$$/\1/p' $(NAME).cabal)

CABAL_INSTALL_DEFARGS ?= -j --disable-documentation --disable-coverage
CABAL_SANDBOX_CONFIG  := $(TOP)/cabal.sandbox.config

export CABAL_SANDBOX_CONFIG

build:
	cabal build -j

deps: add-sources
	cabal install $(CABAL_INSTALL_DEFARGS) --only-dependencies

install: add-sources
	cabal install $(CABAL_INSTALL_DEFARGS)

include $(TOP)/share/stackage.mk

add-sources: cabal.config
	cabal sandbox add-source $(TOP)/core

configure:
	cabal configure

clean:
	cabal clean
	-rm cabal.config

sdist:
	cabal sdist

upload:
	cabal upload dist/$(NAME)-$(VERSION).tar.gz

doc:
	PACKAGE_NAME=$(NAME) \
 PACKAGE_VERSION=$(VERSION) \
 $(TOP)/script/upload-documentation
