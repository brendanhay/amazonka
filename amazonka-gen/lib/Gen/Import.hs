-- |
-- Module      : Gen.Import
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Gen.Import where

import qualified Control.Lens as Lens
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Gen.Prelude
import Gen.Types

operationImports :: Library -> Operation Identity SData a -> [NS]
operationImports l o =
  List.sort $
    "qualified Network.AWS.Request as Req" :
    "qualified Network.AWS.Response as Res" :
    qualifiedLude :
    qualifiedLens :
    l ^. typesNS :
    l ^. operationModules
      ++ maybeToList (const "qualified Network.AWS.Pager as Page" <$> o ^. opPager)

typeImports :: Library -> [NS]
typeImports l =
  List.sort $
    qualifiedLude :
    qualifiedLens :
    signatureImport (l ^. signatureVersion) :
    l ^. typeModules

sumImports :: Library -> [NS]
sumImports l =
  List.sort $
    qualifiedLude :
    l ^. typeModules

productImports :: Library -> Prod -> [NS]
productImports l p =
  List.sort $
    qualifiedLude :
    qualifiedLens :
    l ^. typeModules
      ++ (Set.toList $ Set.map (l ^. typesNS <>) moduleDependencies)
  where
    moduleDependencies = Set.intersection dependencies moduleShapes
    dependencies = Set.map mkNS $ _prodDeps p
    moduleShapes = Set.fromList (mkNS . typeId . identifier <$> l ^.. shapes . Lens.each)

waiterImports :: Library -> [NS]
waiterImports l =
  List.sort $
    qualifiedLens :
    qualifiedLude :
    "qualified Network.AWS.Waiter as Wait" :
    l ^. typesNS :
    map (operationNS ns . _waitOpName) (l ^.. waiters . Lens.each)
  where
    ns = l ^. libraryNS

signatureImport :: Signature -> NS
signatureImport = \case
  V2 -> "qualified Network.AWS.Sign.V2 as Sign"
  _ -> "qualified Network.AWS.Sign.V4 as Sign"

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

qualifiedLens, qualifiedLude :: NS
qualifiedLens = "qualified Network.AWS.Lens as Lens"
qualifiedLude = "qualified Network.AWS.Prelude as Lude"
