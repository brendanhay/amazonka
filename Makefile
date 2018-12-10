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

# Nix
define clean-nix
	rm -f $(strip $1)/default.nix;
endef

.PHONY: nix-clean
nix-clean:
	$(foreach l,$(LIBRARIES),$(call clean-nix, $l))

define mk-nix
  cd $(strip $1); make nix; cd ..;
endef

.PHONY: nix
nix:
	$(foreach l,$(LIBRARIES),$(call mk-nix, $l))
