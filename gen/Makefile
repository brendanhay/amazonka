SHELL     := /usr/bin/env bash
TOP       := ..
MODEL_DIR := model
OUT_DIR   := ..

define version =
$(shell sed -n 's/^version: *\(.*\)$$/\1/p' $(1))
endef

VERSION        = $(call version,amazonka-gen.cabal)
CLIENT_VERSION = $(call version,$(TOP)/amazonka/amazonka.cabal)
CORE_VERSION   = $(call version,$(TOP)/core/amazonka-core.cabal)

MODELS ?= $(sort $(dir $(wildcard $(MODEL_DIR)/*/*)))

.PHONY: gen format

release: gen format

# Relying on install since it seems the simplest path to finding out where
# the stack built binary lives.
gen: install
	~/.local/bin/amazonka-gen \
 --out=$(OUT_DIR) \
 --library-version=$(VERSION) \
 --client-version=$(VERSION) \
 --core-version=$(CORE_VERSION) \
 --annexes=annex \
 --configs=config \
 --templates=template \
 --static=static \
 --retry=$(MODEL_DIR)/_retry.json \
 $(addprefix --model=,$(MODELS))

format:
	@echo -e '\nFormatting...'
	@find $(wildcard $(OUT_DIR)/amazonka-*/gen) \
 -type f \
 -name '*.hs' \
 -printf ' -> %p\n' \
 -exec stylish-haskell -i {} \;

build: $(MODEL_DIR)
	stack build amazonka-gen

install: build
	stack install

clean:
	stack clean
	rm -rf $(MODEL_DIR)

full-clean: clean
	rm -rf vendor

$(MODEL_DIR): vendor/botocore
	cp -rf vendor/botocore/botocore/data $@

vendor/botocore:
	git clone git@github.com:boto/botocore $@
