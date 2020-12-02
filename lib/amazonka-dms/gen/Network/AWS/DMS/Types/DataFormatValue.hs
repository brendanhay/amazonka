{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.DataFormatValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.DataFormatValue where

import Network.AWS.Prelude

data DataFormatValue
  = CSV
  | Parquet
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

instance FromText DataFormatValue where
  parser =
    takeLowerText >>= \case
      "csv" -> pure CSV
      "parquet" -> pure Parquet
      e ->
        fromTextError $
          "Failure parsing DataFormatValue from value: '" <> e
            <> "'. Accepted values: csv, parquet"

instance ToText DataFormatValue where
  toText = \case
    CSV -> "csv"
    Parquet -> "parquet"

instance Hashable DataFormatValue

instance NFData DataFormatValue

instance ToByteString DataFormatValue

instance ToQuery DataFormatValue

instance ToHeader DataFormatValue

instance ToJSON DataFormatValue where
  toJSON = toJSONText

instance FromJSON DataFormatValue where
  parseJSON = parseJSONText "DataFormatValue"
