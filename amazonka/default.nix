{ mkDerivation, amazonka-core, base, bytestring, conduit
, conduit-extra, directory, exceptions, http-client, http-conduit
, http-types, ini, mmorph, monad-control, mtl, resourcet, retry
, stdenv, tasty, tasty-hunit, text, time, transformers
, transformers-base, transformers-compat, unliftio-core, void
}:
mkDerivation {
  pname = "amazonka";
  version = "1.6.0";
  src = ./.;
  libraryHaskellDepends = [
    amazonka-core base bytestring conduit conduit-extra directory
    exceptions http-client http-conduit http-types ini mmorph
    monad-control mtl resourcet retry text time transformers
    transformers-base transformers-compat unliftio-core void
  ];
  testHaskellDepends = [ base tasty tasty-hunit ];
  homepage = "https://github.com/brendanhay/amazonka";
  description = "Comprehensive Amazon Web Services SDK";
  license = stdenv.lib.licenses.mpl20;
}
