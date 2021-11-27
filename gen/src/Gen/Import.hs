module Gen.Import where

import qualified Control.Lens as Lens
import qualified Data.List as List
import qualified Data.Set as Set
import Gen.Prelude
import Gen.Types

operationImports :: Library -> Operation Identity SData a -> [NS]
operationImports l _o =
  Set.toList . Set.fromList $
    "qualified Amazonka.Request as Request" :
    "qualified Amazonka.Response as Response" :
    "qualified Amazonka.Lens as Lens" :
    "qualified Amazonka.Core as Core" :
    "qualified Amazonka.Prelude as Prelude" :
    l ^. typesNS :
    l ^. operationModules

typeImports :: Library -> [NS]
typeImports l =
  List.sort $
    "qualified Amazonka.Lens as Lens" :
    "qualified Amazonka.Core as Core" :
    "qualified Amazonka.Prelude as Prelude" :
    signatureImport (l ^. signatureVersion) :
    l ^. typeModules

lensImports :: Library -> [NS]
lensImports l =
  l ^. typeModules

sumImports :: Library -> [NS]
sumImports l =
  List.sort $
    "qualified Amazonka.Core as Core" :
    "qualified Amazonka.Prelude as Prelude" :
    l ^. typeModules

productImports :: Library -> Prod -> [NS]
productImports l p =
  List.sort $
    "qualified Amazonka.Lens as Lens" :
    "qualified Amazonka.Core as Core" :
    "qualified Amazonka.Prelude as Prelude" :
    l ^. typeModules
      ++ productDependencies l p

productDependencies :: Library -> Prod -> [NS]
productDependencies l p =
  Set.toList (Set.map (l ^. typesNS <>) moduleDependencies)
  where
    moduleDependencies = Set.intersection dependencies (moduleShapes l)
    dependencies = Set.map mkNS (_prodDeps p)

moduleShapes :: Library -> Set.Set NS
moduleShapes l =
  Set.fromList $
    map (mkNS . typeId . identifier) (l ^.. shapes . Lens.each)

waiterImports :: Library -> [NS]
waiterImports l =
  List.sort $
    "qualified Amazonka.Lens as Lens" :
    "qualified Amazonka.Core as Core" :
    "qualified Amazonka.Prelude as Prelude" :
    l ^. typesNS :
    l ^. lensNS :
    map (operationNS ns . _waitOpName) (l ^.. waiters . Lens.each)
  where
    ns = l ^. libraryNS

signatureImport :: Signature -> NS
signatureImport = \case
  V2 -> "qualified Amazonka.Sign.V2 as Sign"
  _ -> "qualified Amazonka.Sign.V4 as Sign"

testImports :: Library -> [NS]
testImports l =
  [ mkNS $ "Test.Amazonka." <> l ^. serviceAbbrev,
    mkNS $ "Test.Amazonka." <> l ^. serviceAbbrev <> ".Internal"
  ]

fixtureImports :: Library -> [NS]
fixtureImports l =
  [ l ^. libraryNS,
    mkNS $ "Test.Amazonka." <> l ^. serviceAbbrev <> ".Internal"
  ]
