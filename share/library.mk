SHELL   := /usr/bin/env bash
NAME    ?= $(notdir $(CURDIR:a/%=%))
VERSION ?= $(shell sed -n 's/^version: *\(.*\)$$/\1/p' $(NAME).cabal)

default:

sdist:
	cabal sdist

upload:
# currently a noop until a blacklist (such as ignoring dynamodb-streams)
# can be put in place.
# cabal upload dist/$(NAME)-$(VERSION).tar.gz
