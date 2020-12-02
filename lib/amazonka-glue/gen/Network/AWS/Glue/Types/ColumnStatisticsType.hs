{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ColumnStatisticsType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ColumnStatisticsType where

import Network.AWS.Prelude

data ColumnStatisticsType
  = Binary
  | Boolean
  | Date
  | Decimal
  | Double
  | Long
  | String
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

instance FromText ColumnStatisticsType where
  parser =
    takeLowerText >>= \case
      "binary" -> pure Binary
      "boolean" -> pure Boolean
      "date" -> pure Date
      "decimal" -> pure Decimal
      "double" -> pure Double
      "long" -> pure Long
      "string" -> pure String
      e ->
        fromTextError $
          "Failure parsing ColumnStatisticsType from value: '" <> e
            <> "'. Accepted values: binary, boolean, date, decimal, double, long, string"

instance ToText ColumnStatisticsType where
  toText = \case
    Binary -> "BINARY"
    Boolean -> "BOOLEAN"
    Date -> "DATE"
    Decimal -> "DECIMAL"
    Double -> "DOUBLE"
    Long -> "LONG"
    String -> "STRING"

instance Hashable ColumnStatisticsType

instance NFData ColumnStatisticsType

instance ToByteString ColumnStatisticsType

instance ToQuery ColumnStatisticsType

instance ToHeader ColumnStatisticsType

instance ToJSON ColumnStatisticsType where
  toJSON = toJSONText

instance FromJSON ColumnStatisticsType where
  parseJSON = parseJSONText "ColumnStatisticsType"
