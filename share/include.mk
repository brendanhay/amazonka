ifndef TOP
$(error TOP is not set)
endif

SHELL   := /usr/bin/env bash
FLAGS   := -j --disable-documentation --disable-library-coverage
SANDBOX := $(TOP)/.cabal-sandbox
DEPS    ?= $(TOP)/core

build:
	cabal build $(addprefix -,$(findstring j,$(MAKEFLAGS)))

install: add-sources
	cabal install $(FLAGS) --only-dependencies

add-sources: cabal.sandbox.config
	cabal sandbox add-source --sandbox=$(SANDBOX) $(DEPS)

cabal.sandbox.config:
	cabal sandbox init --sandbox=$(SANDBOX)

clean:
	cabal clean
	rm -f cabal.sandbox.config

doc:
	cabal haddock
