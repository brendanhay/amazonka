{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Gen.V2.TH
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.V2.TH where

import           Control.Applicative
import           Control.Lens
import           Data.CaseInsensitive      (CI)
import           Data.Char
import           Data.Default
import           Data.Function
import qualified Data.HashMap.Strict       as Map
import           Data.Jason.TH
import           Data.Maybe
import           Data.Monoid               hiding (Sum)
import           Data.Ord
import           Data.String
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Text.Manipulate
import           Gen.V2.Naming
import           Language.Haskell.TH
import           Network.HTTP.Types.Method

data TH = TH
    { _jsonCtor  :: Text -> Text
    , _jsonField :: Text -> Text
    , _lensField :: Text -> Text
    }

dec :: TH
dec = TH toSpinal keyName lensName

enc :: TH
enc = undefined

nullary :: TH -> Name -> Q [Dec]
nullary th = deriveJSON (aeson th)

record :: TH -> Name -> Q [Dec]
record th n = concat <$> sequence
    [ makeLensesWith (lenses th lensRules) n
    , nullary th n
    ]

classy :: TH -> Name -> Q [Dec]
classy th n = concat <$> sequence
    [ makeLensesWith (lenses th classyRules) n
    , nullary th n
    ]

aeson :: TH -> Options
aeson th = defaultOptions
    { constructorTagModifier = withText (_jsonCtor th)
    , fieldLabelModifier     = withText (_jsonField th)
    , omitNothingFields      = True
    , allNullaryToStringTag  = True
    }

lenses :: TH -> LensRules -> LensRules
lenses th = lensField .~ const f
  where
    f :: Name -> [DefName]
    f x = [TopName (mkName (withText (_lensField th) (nameBase x)))]

withText :: (Text -> Text) -> String -> String
withText f = Text.unpack . f . Text.pack
