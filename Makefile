SERVICES  ?= $(wildcard amazonka-*)
LIBRARIES ?= core amazonka test $(SERVICES)
FORWARD   := upload upload-docs

build:
	stack build --fast

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
	stack build --fast amazonka

core:
	stack build --fast amazonka-core

$(SERVICES):
	stack build --fast $@
