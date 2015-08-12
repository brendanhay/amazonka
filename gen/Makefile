SHELL     := /usr/bin/env bash
TOP       := ..
BIN       := bin
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

gen: $(BIN)
	$(BIN)/amazonka-gen \
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
	@find $(wildcard $(OUT_DIR)/amazonka-*/gen) $(wildcard $(OUT_DIR)/test/Test/AWS/Gen) \
 -type f \
 -name '*.hs' \
 -printf ' -> %p\n' \
 -exec stylish-haskell -i {} \;

build: $(MODEL_DIR)
	stack build amazonka-gen

$(BIN): $(MODEL_DIR)
	stack install amazonka-gen --local-bin-path $@

clean:
	stack clean
	rm -rf $(MODEL_DIR)

full-clean: clean
	rm -rf vendor

$(MODEL_DIR): vendor/botocore
	cp -rf vendor/botocore/botocore/data $@

vendor/botocore:
	git clone git@github.com:boto/botocore $@
