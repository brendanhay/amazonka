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
GEN    ?= $(addprefix $(OUT_DIR)/amazonka-,$(notdir $(MODELS)))

.PHONY: gen format

release: gen format

gen: $(BIN)/amazonka-gen
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
	@find $(addsuffix /gen,$(GEN)) $(addsuffix /test/Test/AWS/Gen,$(GEN)) \
 -type f \
 -name '*.hs' \
 -printf ' -> %p\n' \
 -exec stylish-haskell -i {} \;

build: $(MODEL_DIR)
	stack build amazonka-gen

$(BIN)/amazonka-gen: $(BIN) build
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
	git clone git@github.com:boto/botocore $@
