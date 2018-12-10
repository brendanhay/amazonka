{ mkDerivation, amazonka-core, amazonka-test, base, bytestring
, stdenv, tasty, tasty-hunit, text, time, unordered-containers
}:
mkDerivation {
  pname = "amazonka-mediapackage";
  version = "1.6.0";
  src = ./.;
  libraryHaskellDepends = [ amazonka-core base ];
  testHaskellDepends = [
    amazonka-core amazonka-test base bytestring tasty tasty-hunit text
    time unordered-containers
  ];
  homepage = "https://github.com/brendanhay/amazonka";
  description = "Amazon Elemental MediaPackage SDK";
  license = stdenv.lib.licenses.mpl20;
}
