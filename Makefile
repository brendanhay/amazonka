include include/common.mk

DIRS    := $(wildcard amazonka-*)
NESTED   = $(foreach dir,$(DIRS),make -C $(dir) $1;)
TARGETS := build test doc

.PHONY: $(DIRS)

$(DIRS):
	make -C $@ build

$(TARGETS):
	$(call NESTED,$@)

install: cabal.sandbox.config
	$(call NESTED,$@)

gen:
	make -C amazonka-gen $@

clean:
	-rm -rf dist cabal.sandbox.config .cabal-sandbox
	$(call NESTED,$@)

cabal.sandbox.config:
	cabal sandbox init
