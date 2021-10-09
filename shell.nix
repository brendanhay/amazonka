{ system ? builtins.currentSystem, ghcVersion ? "8107" }:

let

  pkgs = import ./nix/nixpkgs.nix { inherit system; };

  bazelrc = pkgs.writeText "amazonka-ghc${ghcVersion}-bazelrc" ''
    build --//tools/ghc:version=${ghcVersion}
  '';

  bazel = pkgs.writeScriptBin "bazel" ''
    #!${pkgs.stdenvNoCC.shell}
    export JAVA_HOME="${pkgs.jdk11_headless.home}"
    exec ${pkgs.bazel_4}/bin/bazel --bazelrc="${bazelrc}" "$@"
  '';

  haskellPackages = pkgs.haskell.packages."ghc${ghcVersion}";

  ghc = haskellPackages.ghcWithPackages (self: [
    # Ensure zlib and friends are locatable in the shell.
    self.digest
    self.zlib
  ]);

in pkgs.mkShell {
  buildInputs = [
    bazel
    ghc
    pkgs.cabal-install
    pkgs.coreutils
    pkgs.file
    pkgs.haskellPackages.cabal-fmt
    pkgs.nixfmt
    pkgs.ormolu
    pkgs.parallel
    pkgs.shellcheck
    pkgs.shfmt
  ];
}
