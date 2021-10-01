{
# The build system where packages will be _built_.
system ? builtins.currentSystem
  # Additional nixpkgs.config overrides.
, config ? { }
  # Additional nixpkgs.overlays.
, overlays ? [ ]
  # Source repositories.
, sources ? import ./sources.nix { }
  # The default version of GHC to use for shell tools.
, ghcVersion ? "ghc8107" }:

import sources.nixpkgs {
  inherit system config;

  overlays = overlays ++ [
    (final: prev: rec {
      inherit sources;

      ghc = prev.haskell.compiler.${ghcVersion};
      haskellPackages = prev.haskellPackages.override { inherit ghc; };
    })
  ];
}
