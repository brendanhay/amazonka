SHELL     := /usr/bin/env bash
TOP       := ..
BIN       := bin
MODEL_DIR := model
OUT_DIR   := ..
STYLISH   := $(BIN)/stylish-haskell

define version =
$(shell sed -n 's/^version: *\([0-9]*\.[0-9]*\.[0-9]\).*$$/\1/p' $(1))
endef

VERSION        = $(shell sed -n 's/^version: *\(.*\)$$/\1/p' amazonka-gen.cabal)
CORE_VERSION   = $(call version,$(TOP)/core/amazonka-core.cabal)
CLIENT_VERSION = $(call version,$(TOP)/gen/amazonka-gen.cabal)

MODELS ?= $(sort $(dir $(wildcard $(MODEL_DIR)/*/*)))

.PHONY: gen format $(BIN)/amazonka-gen

release: gen format

gen: $(MODEL_DIR) $(BIN)/amazonka-gen
	$(BIN)/amazonka-gen \
 --out=$(OUT_DIR) \
 --library-version=$(VERSION) \
 --client-version=$(CLIENT_VERSION) \
 --core-version=$(CORE_VERSION) \
 --annexes=annex \
 --configs=config \
 --templates=template \
 --static=static \
 --retry=$(MODEL_DIR)/_retry.json \
 $(addprefix --model=,$(MODELS))

format:
	@echo -e '\nFormatting...'
	@find $(wildcard $(OUT_DIR)/amazonka-*/gen) $(wildcard $(OUT_DIR)/test/Test/AWS/Gen) \
 -type f \
 -name '*.hs' \
 -printf ' -> %p\n' \
 -exec $(STYLISH) -i {} \;

$(BIN)/amazonka-gen: $(BIN)
	stack build amazonka-gen --copy-bins --local-bin-path $(BIN)

$(BIN):
	mkdir $@

clean:
	stack clean
	rm -rf $(MODEL_DIR)

full-clean: clean
	rm -rf vendor

$(MODEL_DIR): vendor/botocore
	cp -rf vendor/botocore/botocore/data $@

vendor/botocore:
	git clone https://github.com/boto/botocore.git $@
