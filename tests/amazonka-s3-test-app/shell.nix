{ pkgs ? import ../../nix/nixpkgs.nix { } }:
pkgs.ourHaskellPackages.shellFor {
  packages = hs: [ hs.amazonka-s3-test-app ];
  buildInputs = [ pkgs.cabal-install ];
  withHoogle = true;
}
