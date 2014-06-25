include include/common.mk

TARGETS := build clean doc
DIRS    := amazonka-gen amazonka
NESTED   = $(foreach dir,$(DIRS),make -C $(dir) $1;)

deafult: build

$(TARGETS):
	$(call NESTED,$@)

gen:
	make -C amazonka-gen $@

install: cabal.sandbox.config
	$(call NESTED,$@)

cabal.sandbox.config:
	cabal sandbox init
