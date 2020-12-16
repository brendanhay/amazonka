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
import qualified Data.HashMap.Strict.InsOrd as HashMap
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Set as Set
import Gen.Prelude
import Gen.Types

qualify :: NS -> Text -> Text
qualify ns alias = "qualified " <> nsDots ns <> " as " <> alias

operationImports :: Library -> Operation Identity SData a -> [Text]
operationImports l o =
  "qualified Network.AWS.Request as Request" :
  "qualified Network.AWS.Response as Response" :
  qualifiedLude :
  qualifiedLens :
  qualify (l ^. typesNS) "Types" :
  map nsDots (l ^. operationModules)
    ++ maybeToList (const "qualified Network.AWS.Pager as Pager" <$> o ^. opPager)

typeImports :: Library -> [Text]
typeImports l =
  qualifiedLude :
  qualifiedLens :
  signatureImport (l ^. signatureVersion) :
  map nsDots (l ^. typeModules)

sumImports :: Library -> [Text]
sumImports l =
  qualifiedLude :
  map (`qualify` "Types") (l ^. typeModules)

productImports :: Library -> Prod -> [Text]
productImports l p =
  qualifiedLude :
  qualifiedLens :
  map (`qualify` "Types") (l ^. typeModules)
    ++ Set.toList moduleDependencies
  where
    moduleDependencies =
      Set.map (\ns -> qualify (l ^. typesNS <> ns) "Types") $
        Set.intersection moduleShapes dependencies

    moduleShapes =
      Set.fromList (mkNS . typeId . identifier <$> HashMap.elems (l ^. shapes))

    dependencies =
      Set.map mkNS (_prodDeps p)

waiterImports :: Library -> [Text]
waiterImports l =
  qualifiedLens :
  qualifiedLude :
  "qualified Network.AWS.Waiter as Waiter" :
  qualify (l ^. typesNS) "Types" :
  map
    (nsDots . operationNS (l ^. libraryNS) . _waitOpName)
    (HashMap.elems (l ^. waiters))

signatureImport :: Signature -> Text
signatureImport = \case
  V2 -> "qualified Network.AWS.Sign.V2 as Sign"
  _ -> "qualified Network.AWS.Sign.V4 as Sign"

testImports :: Library -> [Text]
testImports l =
  [ "Test.AWS." <> l ^. serviceAbbrev,
    "Test.AWS." <> l ^. serviceAbbrev <> ".Internal"
  ]

fixtureImports :: Library -> [Text]
fixtureImports l =
  [ nsDots (l ^. libraryNS),
    "Test.AWS." <> l ^. serviceAbbrev <> ".Internal"
  ]

qualifiedLens, qualifiedLude :: Text
qualifiedLens = "qualified Network.AWS.Lens as Lens"
qualifiedLude = "qualified Network.AWS.Prelude as Core"
