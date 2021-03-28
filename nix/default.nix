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
, crossOverlays ? [ ]
  # The GHC version to use. (compiler-nix-name in haskell.nix)
, ghcVersion ? "ghc8104" }:

let

  finalSources = import ./sources.nix { inherit pkgs; } // sources;

  haskellNix = import finalSources."haskell.nix" {
    sourcesOverride = { hackage = finalSources."hackage.nix"; };
  };

  finalOverlays = haskellNix.overlays ++ [
    # Add top-level `.sources` attribute.
    (_final: _prev: { sources = finalSources; })

    (import ./overlays/haskell.nix)
    (import ./overlays/cabal-project.nix { inherit ghcVersion; })
  ] ++ overlays;

  pkgs = import haskellNix.sources.nixpkgs-2009 (haskellNix.nixpkgsArgs // {
    inherit system crossSystem crossOverlays;

    config = haskellNix.config // config;
    overlays = finalOverlays;
  });

in pkgs
