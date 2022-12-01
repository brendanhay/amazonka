{-# LANGUAGE LambdaCase #-}

module Gen.Import where

import qualified Control.Lens as Lens
import qualified Data.List as List
import qualified Data.Set as Set
import Gen.Prelude
import Gen.Types

operationImports :: Library -> Operation Identity SData a -> [NS]
operationImports l _o =
  Set.toAscList . Set.fromList $
    "qualified Amazonka.Request as Request" :
    "qualified Amazonka.Response as Response" :
    "qualified Amazonka.Core as Core" :
    "qualified Amazonka.Core.Lens.Internal as Lens" :
    "qualified Amazonka.Prelude as Prelude" :
    l ^. typesNS :
    l ^. operationModules

typeImports :: Library -> [NS]
typeImports l =
  List.sort $
    "qualified Amazonka.Core as Core" :
    "qualified Amazonka.Core.Lens.Internal as Lens" :
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
    "qualified Amazonka.Core as Core" :
    "qualified Amazonka.Core.Lens.Internal as Lens" :
    "qualified Amazonka.Prelude as Prelude" :
    l ^. typeModules ++ productDependencies l p

productDependencies :: Library -> Prod -> [NS]
productDependencies l p =
  Set.toList (Set.map buildImport moduleDependencies)
  where
    buildImport t
      | (_prodName p, t) `elem` (l ^. cuts') = addSource t'
      | otherwise = t'
      where
        t' = l ^. typesNS <> mkNS t
        addSource = \case
          NS (n : ns) -> NS $ "{-# SOURCE #-} " <> n : ns
          NS [] -> NS []

    moduleDependencies =
      Set.intersection dependencies (Set.map typeId moduleShapes)
    dependencies = _prodDeps p

    moduleShapes =
      Set.fromList $ l ^.. shapes . traverse . Lens.to identifier

waiterImports :: Library -> [NS]
waiterImports l =
  List.sort $
    "qualified Amazonka.Core as Core" :
    "qualified Amazonka.Core.Lens.Internal as Lens" :
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
