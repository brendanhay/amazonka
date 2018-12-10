{ mkDerivation, aeson, amazonka, amazonka-core, amazonka-kms
, amazonka-s3, base, bytestring, case-insensitive, conduit
, cryptonite, exceptions, lens, memory, mtl, stdenv, text
, unordered-containers
}:
mkDerivation {
  pname = "amazonka-s3-encryption";
  version = "1.6.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson amazonka amazonka-core amazonka-kms amazonka-s3 base
    bytestring case-insensitive conduit cryptonite exceptions lens
    memory mtl text unordered-containers
  ];
  homepage = "https://github.com/brendanhay/amazonka";
  description = "Amazon Simple Storage Service SDK - Client-Side Encryption";
  license = "unknown";
}
