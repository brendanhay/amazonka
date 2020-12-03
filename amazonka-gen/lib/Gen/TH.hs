{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Gen.TH
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Gen.TH where

import qualified Control.Lens as Lens
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.Text as Text
import qualified Data.Text.Manipulate as Manipulate
import GHC.Generics
import Gen.Prelude
import Gen.Text

data TH = TH
  { _ctor :: Text -> Text,
    _field :: Text -> Text,
    _lenses :: Bool
  }

$(Lens.makeLenses ''TH)

genericParseJSON ::
  (Generic a, Aeson.GFromJSON Aeson.Zero (Rep a)) =>
  TH ->
  Aeson.Value ->
  Aeson.Types.Parser a
genericParseJSON th = Aeson.genericParseJSON (options th)

genericToJSON ::
  (Generic a, Aeson.GToJSON Aeson.Zero (Rep a)) =>
  TH ->
  a ->
  Aeson.Value
genericToJSON th = Aeson.genericToJSON (options th)

upper, lower, spinal, camel :: TH
upper = TH Text.toUpper Text.toUpper False
lower = TH Text.toLower Text.toLower False
spinal = TH Manipulate.toSpinal Manipulate.toSpinal False
camel = TH Manipulate.toCamel Manipulate.toCamel False

options :: TH -> Aeson.Options
options TH {..} =
  Aeson.defaultOptions
    { Aeson.constructorTagModifier = f _ctor,
      Aeson.fieldLabelModifier = f _field,
      Aeson.allNullaryToStringTag = True,
      Aeson.sumEncoding =
        Aeson.TaggedObject
          { Aeson.tagFieldName = "type",
            Aeson.contentsFieldName = "contents"
          }
    }
  where
    f g = asText (g . camelAcronym . h . stripSuffix "'")

    h
      | _lenses = stripLens
      | otherwise = stripPrefix "_"
