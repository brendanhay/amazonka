{ system ? builtins.currentSystem, ghcVersion ? "ghc8107" }:

let

  pkgs = import ./nix/nixpkgs.nix { inherit system ghcVersion; };

in pkgs.mkShell {
  buildInputs = [
    pkgs.bazel
    pkgs.cabal-fmt
    pkgs.cabal-install
    pkgs.coreutils
    pkgs.file
    pkgs.ghc
    pkgs.niv
    pkgs.nixfmt
    pkgs.ormolu
    pkgs.parallel
    pkgs.shellcheck
    pkgs.shfmt
  ];

  shellHook = ''
    export BOTOCORE_PATH='${pkgs.sources.botocore}/botocore/data';
  '';
}
