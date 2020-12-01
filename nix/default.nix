# The build system where packages will be _built_.
{ system ? builtins.currentSystem
  # The host system where packages will _run_.
, crossSystem ? null
  # Additional sources.json overrides.
, sources ? { }
  # Additional nixpkgs.config overrides.
, config ? { }
  # Additional nixpkgs.overlays.
, overlays ? [ ]
  # Overlays to apply to the last package set in cross compilation.
, crossOverlays ? [ ] }:

let

  finalSources = import ./sources.nix { inherit pkgs; } // sources;

  haskellNix = import finalSources."haskell.nix" {
    sourcesOverride = { hackage = finalSources."hackage.nix"; };
  };

  finalOverlays = haskellNix.overlays ++ [
    # Add top-level `.sources` attribute.
    (_final: _prev: { sources = finalSources; })

    # Basic library functions.
    (import ./overlays/lib-local.nix)

    # haskell.nix overrides + extensions.
    (import ./overlays/haskell.nix)
  ] ++ overlays;

  pkgs = import finalSources.nixpkgs {
    inherit system crossSystem crossOverlays;

    config = haskellNix.config // config;
    overlays = finalOverlays;
  };

in pkgs
