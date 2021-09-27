# The build system where packages will be _built_.
{ system ? builtins.currentSystem
  # Additional sources.json overrides.
, sources ? { }
  # Additional nixpkgs.config overrides.
, config ? { }
  # Additional nixpkgs.overlays.
, overlays ? [ ]
  # The GHC version to use. (compiler-nix-name in haskell.nix)
, ghcVersion ? "ghc8107" }:

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

  pkgs = import haskellNix.sources.nixpkgs-2105 (haskellNix.nixpkgsArgs // {
    inherit system;

    config = haskellNix.config // config;
    overlays = finalOverlays;
  });

in pkgs
