{ system ? builtins.currentSystem, config ? { }, overlays ? [ ]
, sources ? import ./sources.nix { } }:

import sources.nixpkgs-latest { inherit system config overlays; }
