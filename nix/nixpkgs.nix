{ system ? builtins.currentSystem
, config ? { }
, overlays ? [ ]
, sources ? import ./sources.nix { }
}:
import sources.nixpkgs {
  inherit system config;
  overlays = [ (import ./haskell.nix) ] ++ overlays;
}
