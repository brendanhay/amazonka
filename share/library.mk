SHELL   := /usr/bin/env bash
NAME    ?= $(notdir $(CURDIR:a/%=%))
VERSION ?= $(shell sed -n 's/^version: *\(.*\)$$/\1/p' $(NAME).cabal)

default:

sdist:
	cabal sdist

upload:
	cabal upload dist/$(NAME)-$(VERSION).tar.gz

upload-docs:
	PACKAGE=$(NAME) ../script/hackage-documentation
