{ system ? builtins.currentSystem, ghcVersion ? "8107" }:

let

  pkgs = import ./nix/nixpkgs.nix { inherit system; };

  kernel = if pkgs.stdenv.targetPlatform.isLinux then
    "linux"
  else if pkgs.stdenv.targetPlatform.isDarwin then
    "darwin"
  else
    throw "unsupported system";

  bazelrc = pkgs.writeText "amazonka-ghc${ghcVersion}-bazelrc" ''
    build --config=${kernel}
    build --//tools/ghc:version=${ghcVersion}
  '';

  bazel = pkgs.writeScriptBin "bazel" ''
    #!${pkgs.stdenvNoCC.shell}
    export JAVA_HOME="${pkgs.jdk11_headless.home}"
    exec ${pkgs.bazel_4}/bin/bazel --bazelrc="${bazelrc}" "$@"
  '';

  # Ensure zlib and friends are locatable if you use cabal in the nix-shell.
  ghc = pkgs.haskell.packages."ghc${ghcVersion}".ghcWithPackages
    (self: [ self.digest self.zlib ]);

in pkgs.mkShell {
  buildInputs = [
    bazel
    ghc
    pkgs.python3
    pkgs.cabal-install
    pkgs.haskellPackages.cabal-fmt
    pkgs.curl
    pkgs.ormolu
    pkgs.nixfmt
    pkgs.shfmt
    pkgs.shellcheck
    pkgs.parallel
  ];
}

