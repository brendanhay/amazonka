{ mkDerivation, amazonka-core, amazonka-test, base, bytestring
, lens, stdenv, tasty, tasty-hunit, text, time
, unordered-containers
}:
mkDerivation {
  pname = "amazonka-s3";
  version = "1.6.0";
  src = ./.;
  libraryHaskellDepends = [ amazonka-core base lens text ];
  testHaskellDepends = [
    amazonka-core amazonka-test base bytestring tasty tasty-hunit text
    time unordered-containers
  ];
  homepage = "https://github.com/brendanhay/amazonka";
  description = "Amazon Simple Storage Service SDK";
  license = stdenv.lib.licenses.mpl20;
}
