{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.ParquetCompression
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.ParquetCompression where

import Network.AWS.Prelude

data ParquetCompression
  = PCGzip
  | PCSnappy
  | PCUncompressed
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

instance FromText ParquetCompression where
  parser =
    takeLowerText >>= \case
      "gzip" -> pure PCGzip
      "snappy" -> pure PCSnappy
      "uncompressed" -> pure PCUncompressed
      e ->
        fromTextError $
          "Failure parsing ParquetCompression from value: '" <> e
            <> "'. Accepted values: gzip, snappy, uncompressed"

instance ToText ParquetCompression where
  toText = \case
    PCGzip -> "GZIP"
    PCSnappy -> "SNAPPY"
    PCUncompressed -> "UNCOMPRESSED"

instance Hashable ParquetCompression

instance NFData ParquetCompression

instance ToByteString ParquetCompression

instance ToQuery ParquetCompression

instance ToHeader ParquetCompression

instance ToJSON ParquetCompression where
  toJSON = toJSONText

instance FromJSON ParquetCompression where
  parseJSON = parseJSONText "ParquetCompression"
