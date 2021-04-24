# The build system where packages will be built.
{ system ? builtins.currentSystem
  # The host system where packages will run.
, crossSystem ? null
  # Additional sources.json overrides.
, sources ? { }
  # Additional nixpkgs.config overrides.
, config ? { }
  # Additional nixpkgs.overlays.
, overlays ? [ ]
  # Overlays to apply to the last package set in cross compilation.
, crossOverlays ? [ ]
  # The GHC version to use. (compiler-nix-name in haskell.nix)
, ghcVersion ? "ghc8104"
  # The names of the models to generate - ie. [ "ec2", "s3" ]
  # Setting to an empty list will use file names from ./config
, models ? [ ]
  # Whether the formatPhase (cabal-fmt, ormolu) should be run.
, format ? true
  # Report botocore service definitions lacking amazonka configuration.
, audit ? false }:

let

  pkgs = import ./nix/default.nix {
    inherit system sources config overlays crossOverlays ghcVersion;
  };

  inherit (pkgs) lib cabalProject;

  botocore = pkgs.sources.botocore;
  botocoreDir = "${botocore}/botocore/data";
  botocoreRev = builtins.substring 0 7 botocore.rev;

  annexesDir = ./config/annexes;
  servicesDir = ./config/services;
  templatesDir = ./config/templates;
  assetsDir = ./config/assets;

  clientVersion = cabalProject.amazonka.identifier.version;
  coreVersion = cabalProject.amazonka-core.identifier.version;
  libraryVersion = cabalProject.amazonka-gen.identifier.version;

  configNames = dir:
    let names = builtins.attrNames (builtins.readDir dir);
    in builtins.map (lib.removeSuffix ".json") names;

  modelNames = let
    # The available botocore service definitions.
    available = configNames botocoreDir;
    # The available amazonka config files.
    configured = configNames servicesDir;
    # Botocore service definitions _without_ a corresponding amazonka config.
    unconfigured = lib.subtractLists configured available;
    # Supplied 'models' arguments that don't correspond to an amazonka config.
    missing = lib.subtractLists configured models;
    # Check the selected model names or default to all, if necessary.
    selected = if models == [ ] then
      configured
    else if missing == [ ] then
      models
    else
      throw ''
        Unknown model(s): ${lib.concatStringsSep ", " missing}
      '';
    # Report any unconfigured botocore service definitions.
  in lib.traceIf (audit && unconfigured != [ ]) ''
    Unconfigured model(s):
     - ${lib.concatStringsSep "\n - " unconfigured}
  '' selected;

  modelArguments =
    builtins.map (v: ''--model="${botocoreDir + "/${v}"}"'') modelNames;

in pkgs.stdenvNoCC.mkDerivation {
  pname = "amazonka";
  version = botocoreRev;

  phases = [ "generatePhase" ] ++ lib.optionals format [ "formatPhase" ];

  buildInputs = [
    (pkgs.haskell-nix.tool ghcVersion "ormolu" "0.1.4.1")
    (pkgs.haskell-nix.tool ghcVersion "cabal-fmt" "0.1.5.1")
    cabalProject.amazonka-gen.components.exes.amazonka-gen
  ];

  generatePhase = ''
    mkdir -p $out

    args=(
      --out=$out \
      --library-version=${libraryVersion} \
      --client-version=${clientVersion} \
      --core-version=${coreVersion} \
      --annexes="${annexesDir}" \
      --services="${servicesDir}" \
      --templates="${templatesDir}" \
      --assets="${assetsDir}" \
      --retry=${botocoreDir}/_retry.json \
      ${builtins.concatStringsSep " " modelArguments}
    )

    # Reproduce, without re-running nix.
    echo "cabal new-run amazonka-gen -- ''${args[@]}"

    amazonka-gen +RTS -N1 -A128m -qg -RTS "''${args[@]}"
  '';

  formatPhase = ''
    export LC_ALL=C.UTF-8

    cd $out

    for dir in amazonka*; do
      echo " -> Formatting $dir"

      find $dir -type f -name '*.cabal' -print0 \
        | xargs -0 cabal-fmt --inplace --indent=2

      find $dir -type f -name '*.hs' -print0 \
        | xargs -0 ormolu --mode=inplace \
          --ghc-opt='-XPatternSynonyms' \
          --ghc-opt='-XTypeApplications'
    done
  '';
}
