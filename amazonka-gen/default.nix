{ lib, stdenvNoCC, cabal-fmt, ormolu, botocore, amazonka-gen, libraryVersion
, clientVersion, coreVersion, models }:

let

  botocoreData = "${botocore}/botocore/data";

  modelArguments =
    builtins.concatStringsSep " "
      (builtins.map (v: ''--model="${botocoreData + "/${v}"}"'') models);

in stdenvNoCC.mkDerivation {
  pname = "amazonka-gen";
  version = botocore.rev;
  phases = [ "generatePhase" "formatPhase" ];

  generatePhase = ''
    mkdir $out

    echo ${amazonka-gen}/bin/amazonka-gen \
      --out=$out \
      --library-version=${libraryVersion} \
      --client-version=${clientVersion} \
      --core-version=${coreVersion} \
      --annexes="${./annex}" \
      --configs="${./config}" \
      --templates="${./template}" \
      --static="${./static}" \
      --retry=${botocoreData}/_retry.json \
      ${modelArguments}
  '';

  formatPhase = ''
    find $out \
      -type f -name '*.cabal' \
      -exec cabal-fmt --inplace --indent=2 {} \+

    find $out \
      -type f -name '*.hs' \
      -exec ormolu -i \
      {} \+
  '';
}
