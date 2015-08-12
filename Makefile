SERVICES  ?= $(wildcard amazonka-*)
LIBRARIES ?= core test amazonka $(SERVICES)
FORWARD   := sdist upload

build:
	stack build

clean:
	stack clean

define forward
$1: $$(addprefix $1-,$$(LIBRARIES))

$1-%:
	@make -C $$* $1

.PHONY: $1
endef

$(foreach c,$(FORWARD),$(eval $(call forward, $c)))

.PHONY: $(LIBRARIES)

amazonka:
	stack build amazonka

core:
	stack build amazonka-core

test:
	stack test

$(SERVICES):
	stack build $@
