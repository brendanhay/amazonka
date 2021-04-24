{-# LANGUAGE TemplateHaskell #-}

-- Module      : Gen.TH
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)

module Gen.TH
  ( gParseJSON',
    gToJSON',
    TH,
    field,
    ctor,
    lenses,
    upper,
    lower,
    spinal,
    camel,
  )
where

import Control.Lens
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Manipulate qualified as Text
import GHC.Generics
import Gen.Text

data TH = TH
  { _ctor :: Text -> Text,
    _field :: Text -> Text,
    _lenses :: Bool
  }

makeLenses ''TH

gParseJSON' :: (Generic a, GFromJSON Zero (Rep a)) => TH -> Value -> Parser a
gParseJSON' th = genericParseJSON (aeson th)

gToJSON' :: (Generic a, GToJSON Zero (Rep a)) => TH -> a -> Value
gToJSON' th = genericToJSON (aeson th)

upper, lower, spinal, camel :: TH
upper = TH Text.toUpper Text.toUpper False
lower = TH Text.toLower Text.toLower False
spinal = TH Text.toSpinal Text.toSpinal False
camel = TH Text.toCamel Text.toCamel False

aeson :: TH -> Options
aeson TH {..} =
  defaultOptions
    { constructorTagModifier = f _ctor,
      fieldLabelModifier = f _field,
      allNullaryToStringTag = True,
      sumEncoding =
        TaggedObject
          { tagFieldName = "type",
            contentsFieldName = "contents"
          }
    }
  where
    f g = asText (g . camelAcronym . h . stripSuffix "'")

    h
      | _lenses = stripLens
      | otherwise = stripPrefix "_"
