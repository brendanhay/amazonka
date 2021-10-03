{
# The build system where packages will be _built_.
system ? builtins.currentSystem
  # Additional nixpkgs.config overrides.
, config ? { }
  # Additional nixpkgs.overlays.
, overlays ? [ ]
  # Source repositories.
, sources ? import ./sources.nix { }
}:

import sources.nixpkgs {
  inherit system config overlays;
}
