{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.ParquetVersionValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.ParquetVersionValue where

import Network.AWS.Prelude

data ParquetVersionValue
  = Parquet10
  | Parquet20
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

instance FromText ParquetVersionValue where
  parser =
    takeLowerText >>= \case
      "parquet-1-0" -> pure Parquet10
      "parquet-2-0" -> pure Parquet20
      e ->
        fromTextError $
          "Failure parsing ParquetVersionValue from value: '" <> e
            <> "'. Accepted values: parquet-1-0, parquet-2-0"

instance ToText ParquetVersionValue where
  toText = \case
    Parquet10 -> "parquet-1-0"
    Parquet20 -> "parquet-2-0"

instance Hashable ParquetVersionValue

instance NFData ParquetVersionValue

instance ToByteString ParquetVersionValue

instance ToQuery ParquetVersionValue

instance ToHeader ParquetVersionValue

instance ToJSON ParquetVersionValue where
  toJSON = toJSONText

instance FromJSON ParquetVersionValue where
  parseJSON = parseJSONText "ParquetVersionValue"
