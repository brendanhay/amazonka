-- Module      : Gen.Import
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)

module Gen.Import where

import Control.Lens
import Data.List (sort)
import Data.Maybe
import qualified Data.Set as Set
import Gen.Types

operationImports :: Library -> Operation Identity SData a -> [NS]
operationImports l o =
  Set.toList . Set.fromList $
    "qualified Network.AWS.Internal.Request as Request" :
    "qualified Network.AWS.Internal.Response as Response" :
    "qualified Network.AWS.Internal.Lens as Lens" :
    "qualified Network.AWS.Internal.Prelude as Prelude" :
    l ^. typesNS :
    l ^. operationModules
      ++ maybeToList (const "qualified Network.AWS.Pager as Pager" <$> o ^. opPager)

typeImports :: Library -> [NS]
typeImports l =
  sort $
    "qualified Network.AWS.Internal.Lens as Lens" :
    "qualified Network.AWS.Internal.Prelude as Prelude" :
    signatureImport (l ^. signatureVersion) :
    l ^. typeModules

lensImports :: Library -> [NS]
lensImports l =
  l ^. typeModules

sumImports :: Library -> [NS]
sumImports l =
  sort $
    "qualified Network.AWS.Internal.Prelude as Prelude" :
    l ^. typeModules

productImports :: Library -> Prod -> [NS]
productImports l p =
  sort $
    "qualified Network.AWS.Internal.Lens as Lens" :
    "qualified Network.AWS.Internal.Prelude as Prelude" :
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
    map (mkNS . typeId . identifier) (l ^.. shapes . each)

waiterImports :: Library -> [NS]
waiterImports l =
  sort $
    "qualified Network.AWS.Internal.Lens as Lens" :
    "qualified Network.AWS.Internal.Prelude as Prelude" :
    "qualified Network.AWS.Waiter as Waiter" :
    l ^. typesNS :
    l ^. lensNS :
    map (operationNS ns . _waitOpName) (l ^.. waiters . each)
  where
    ns = l ^. libraryNS

signatureImport :: Signature -> NS
signatureImport = \case
  V2 -> "qualified Network.AWS.Internal.Sign.V2 as Sign"
  _ -> "qualified Network.AWS.Internal.Sign.V4 as Sign"

testImports :: Library -> [NS]
testImports l =
  [ mkNS $ "Test.AWS." <> l ^. serviceAbbrev,
    mkNS $ "Test.AWS." <> l ^. serviceAbbrev <> ".Internal"
  ]

fixtureImports :: Library -> [NS]
fixtureImports l =
  [ l ^. libraryNS,
    mkNS $ "Test.AWS." <> l ^. serviceAbbrev <> ".Internal"
  ]
