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
    camel,
  )
where

import Control.Lens
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Text (Text)
import qualified Data.Text as Text
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

upper, lower, camel :: TH
upper = TH Text.toUpper Text.toUpper False
lower = TH Text.toLower Text.toLower False
camel = TH toCamelCase toCamelCase False

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
    f g = asText (g . h . stripSuffix "'")

    h
      | _lenses = stripLens
      | otherwise = stripPrefix "_"

asText :: (Text -> Text) -> String -> String
asText f = Text.unpack . f . Text.pack
