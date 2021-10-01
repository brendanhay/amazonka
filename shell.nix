{ system ? builtins.currentSystem, ghcVersion ? "ghc8107" }:

let

  pkgs = import ./nix/nixpkgs.nix { inherit system ghcVersion; };

  # Entering a nix-shell and running bazel will use the same GHC versions
  # because of this rc snippet.
  bazelrc = pkgs.writeText "amazonka-bazelrc" ''
    build --host_platform=//tools/platforms:${ghcVersion}
  '';

  bazel = pkgs.writeScriptBin "bazel" (''
    #!${pkgs.bash}/bin/bash
  '' + pkgs.lib.optionalString (pkgs.buildPlatform.libc == "glibc") ''
    export LOCALE_ARCHIVE="${pkgs.glibcLocales}/lib/locale/locale-archive"
  '' + ''
    exec ${pkgs.bazel_4}/bin/bazel --bazelrc "${bazelrc}" "$@"
  '');

in pkgs.mkShell {
  buildInputs = [
    bazel
    pkgs.cabal-install
    pkgs.coreutils
    pkgs.file
    pkgs.ghc
    pkgs.haskellPackages.cabal-fmt
    pkgs.niv
    pkgs.nixfmt
    pkgs.ormolu
    pkgs.parallel
    pkgs.shfmt
    pkgs.shellcheck
  ];

  shellHook = ''
    export BOTOCORE_PATH='${pkgs.sources.botocore}/botocore/data';
  '';
}
