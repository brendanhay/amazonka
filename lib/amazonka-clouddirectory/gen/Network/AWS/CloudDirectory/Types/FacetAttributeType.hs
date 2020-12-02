{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.FacetAttributeType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.FacetAttributeType where

import Network.AWS.Prelude

data FacetAttributeType
  = Binary
  | Boolean
  | Datetime
  | Number
  | String
  | Variant
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Enum,
      Bounded,
      Data,
      Typeable,
      Generic
    )

instance FromText FacetAttributeType where
  parser =
    takeLowerText >>= \case
      "binary" -> pure Binary
      "boolean" -> pure Boolean
      "datetime" -> pure Datetime
      "number" -> pure Number
      "string" -> pure String
      "variant" -> pure Variant
      e ->
        fromTextError $
          "Failure parsing FacetAttributeType from value: '" <> e
            <> "'. Accepted values: binary, boolean, datetime, number, string, variant"

instance ToText FacetAttributeType where
  toText = \case
    Binary -> "BINARY"
    Boolean -> "BOOLEAN"
    Datetime -> "DATETIME"
    Number -> "NUMBER"
    String -> "STRING"
    Variant -> "VARIANT"

instance Hashable FacetAttributeType

instance NFData FacetAttributeType

instance ToByteString FacetAttributeType

instance ToQuery FacetAttributeType

instance ToHeader FacetAttributeType

instance ToJSON FacetAttributeType where
  toJSON = toJSONText

instance FromJSON FacetAttributeType where
  parseJSON = parseJSONText "FacetAttributeType"
