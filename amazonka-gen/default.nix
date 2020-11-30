{ lib,
  stdenvNoCC,
  haskell-nix,
  amazonka,
  amazonka-core,
  amazonka-gen,
  botocore,
  models
}:

let

  botocoreData = "${botocore}/botocore/data";
    
  modelVersion = model:
    let filter = _name: type: type == "directory";
        contents = builtins.readDir (botocoreData + "/${model}");
        versions = builtins.attrNames (lib.filterAttrs filter contents);
    in builtins.head (lib.reverseList versions);

  modelArguments =
    builtins.concatStringsSep " "
      (builtins.map (v: "--model=${modelVersion v}") models);

in stdenvNoCC.mkDerivation {
  pname = amazonka-gen.identifier.name;
  version = "${amazonka-gen.identifier.version}-${botocore.rev}";

  builder = ''
    mkdir $out

    ${amazonka-gen.components.exes.amazonka-gen}/bin/amazonka-gen \
      --out=$out \
      --library-version=${amazonka-gen.identifier.version} \
      --client-version=${amazonka.identifier.version} \
      --core-version=${amazonka-core.identifier.version} \ 
      --annexes=${./annex} \
      --configs=${./config} \
      --templates=${./template} \
      --static=${./static} \
      --retry=${botocoreData}/_retry.json \
      ${modelArguments}

    cabal-fmt
    ormolu
  ''; 
}
