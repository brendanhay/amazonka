DEPS ?= core $(wildcard amazonka-*)

.PHONY: full-clean

define forward
$1: cabal.sandbox.config $$(addprefix $1-,$$(DEPS))

$1-%:
	@make -C $$* $1

.PHONY: $1
endef

FORWARD := build configure deps install sdist upload clean doc

$(foreach c,$(FORWARD),$(eval $(call forward, $c)))

full-clean: clean
	rm -rf cabal.sandbox.config .cabal-sandbox

cabal.sandbox.config:
	cabal sandbox init

travis: cabal.sandbox.config
	@make -C core test
