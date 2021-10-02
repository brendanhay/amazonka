{ system ? builtins.currentSystem, ghcVersion ? "ghc8107" }:

let

  pkgs = import ./nix/nixpkgs.nix { inherit system ghcVersion; };

  # Ensure zlib and friends are correctly handled.
  ghc = pkgs.haskellPackages.ghcWithPackages (self: [
    self.digest
    self.zlib
  ]);

in pkgs.mkShell {
  nativeBuildInputs = [
    ghc
    pkgs.bazel
    pkgs.cabal-install
    pkgs.coreutils
    pkgs.file
    pkgs.haskellPackages.cabal-fmt
    pkgs.niv
    pkgs.nixfmt
    pkgs.ormolu
    pkgs.parallel
    pkgs.shellcheck
    pkgs.shfmt
  ];
}
