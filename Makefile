include include/common.mk

DIRS  := $(wildcard amazonka-*)
NESTED = $(foreach dir,$(DIRS),make -C $(dir) $1;)

.PHONY: $(DIRS)

$(DIRS):
	make -C $@ build

install: cabal.sandbox.config
	$(call NESTED,$@)

clean:
	-rm -rf dist cabal.sandbox.config .cabal-sandbox
	$(call NESTED,$@)

cabal.sandbox.config:
	cabal sandbox init
