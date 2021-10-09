{ system ? builtins.currentSystem, config ? { }, overlays ? [ ]
, sources ? import ./sources.nix { } }:

let

  nixpkgs-2009 = import sources.nixpkgs-2009 { inherit system; };

in import sources.nixpkgs-latest {
  inherit system config;

  overlays = [
    (final: prev: {
      inherit sources;

      haskell = prev.lib.recursiveUpdate prev.haskell {
        compiler.ghc865 = nixpkgs-2009.haskell.compiler.ghc865;
        packages.ghc865 = nixpkgs-2009.haskell.packages.ghc865;
      };
    })
  ] ++ overlays;
}
