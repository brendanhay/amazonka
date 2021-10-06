{ system ? builtins.currentSystem, ghcVersion ? "ghc8107" }:

let

  pkgs = import ./nix/nixpkgs.nix { inherit system; };

  # bazelrc = pkgs.writeText "amazonka-bazelrc-${ghcVersion}" ''
  #   build --host_platform=//tools/platforms:${ghcVersion}
  # '';

  bazel = pkgs.writeScriptBin "bazel" ''
    #!${pkgs.stdenvNoCC.shell}
    export JAVA_HOME="${pkgs.jdk11_headless.home}"
    exec ${pkgs.bazel_4}/bin/bazel "$@"
  '';


  # haskellPackages = pkgs.haskellPackages.override {
  #   ghc = pkgs.haskell.compiler.${ghcVersion};
  #  };

  # # Ensure zlib and friends are locatable in the shell.
  # ghc = haskellPackages.ghcWithPackages (self: [ self.digest self.zlib ]);

in pkgs.mkShell {
  buildInputs = [
    bazel
    # ghc
    # haskellPackages.cabal-fmt
    # pkgs.cabal-install
    # pkgs.coreutils
    # pkgs.file
    # pkgs.niv
    # pkgs.nixfmt
    # pkgs.ormolu
    # pkgs.parallel
    # pkgs.shellcheck
    # pkgs.shfmt
  ];
}
