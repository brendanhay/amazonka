{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.IndexFieldType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.IndexFieldType where

import Network.AWS.Prelude

-- | The type of field. The valid options for a field depend on the field type. For more information about the supported field types, see <http://docs.aws.amazon.com/cloudsearch/latest/developerguide/configuring-index-fields.html Configuring Index Fields> in the /Amazon CloudSearch Developer Guide/ .
data IndexFieldType
  = Date
  | DateArray
  | Double
  | DoubleArray
  | Int
  | IntArray
  | Latlon
  | Literal
  | LiteralArray
  | Text
  | TextArray
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

instance FromText IndexFieldType where
  parser =
    takeLowerText >>= \case
      "date" -> pure Date
      "date-array" -> pure DateArray
      "double" -> pure Double
      "double-array" -> pure DoubleArray
      "int" -> pure Int
      "int-array" -> pure IntArray
      "latlon" -> pure Latlon
      "literal" -> pure Literal
      "literal-array" -> pure LiteralArray
      "text" -> pure Text
      "text-array" -> pure TextArray
      e ->
        fromTextError $
          "Failure parsing IndexFieldType from value: '" <> e
            <> "'. Accepted values: date, date-array, double, double-array, int, int-array, latlon, literal, literal-array, text, text-array"

instance ToText IndexFieldType where
  toText = \case
    Date -> "date"
    DateArray -> "date-array"
    Double -> "double"
    DoubleArray -> "double-array"
    Int -> "int"
    IntArray -> "int-array"
    Latlon -> "latlon"
    Literal -> "literal"
    LiteralArray -> "literal-array"
    Text -> "text"
    TextArray -> "text-array"

instance Hashable IndexFieldType

instance NFData IndexFieldType

instance ToByteString IndexFieldType

instance ToQuery IndexFieldType

instance ToHeader IndexFieldType

instance FromXML IndexFieldType where
  parseXML = parseXMLText "IndexFieldType"
