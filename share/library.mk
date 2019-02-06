SHELL := /usr/bin/env bash
NAME  ?= $(notdir $(CURDIR:a/%=%))

default:

upload:
	stack upload .

upload-docs:
	PACKAGE=$(NAME) ../script/hackage-documentation
