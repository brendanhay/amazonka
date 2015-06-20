SERVICES  ?= $(wildcard amazonka-*)
LIBRARIES ?= core amazonka $(SERVICES)
FORWARD   := sdist upload

build:
	stack build

test:
	stack test amazonka-core

clean:
	stack clean

define forward
$1: $$(addprefix $1-,$$(LIBRARIES)

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

$(SERVICES):
	stack build $@
