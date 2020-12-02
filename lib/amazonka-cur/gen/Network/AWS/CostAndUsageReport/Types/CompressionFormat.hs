{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostAndUsageReport.Types.CompressionFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostAndUsageReport.Types.CompressionFormat where

import Network.AWS.Prelude

-- | The compression format that AWS uses for the report.
data CompressionFormat
  = CFGzip
  | CFParquet
  | CFZip
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

instance FromText CompressionFormat where
  parser =
    takeLowerText >>= \case
      "gzip" -> pure CFGzip
      "parquet" -> pure CFParquet
      "zip" -> pure CFZip
      e ->
        fromTextError $
          "Failure parsing CompressionFormat from value: '" <> e
            <> "'. Accepted values: gzip, parquet, zip"

instance ToText CompressionFormat where
  toText = \case
    CFGzip -> "GZIP"
    CFParquet -> "Parquet"
    CFZip -> "ZIP"

instance Hashable CompressionFormat

instance NFData CompressionFormat

instance ToByteString CompressionFormat

instance ToQuery CompressionFormat

instance ToHeader CompressionFormat

instance ToJSON CompressionFormat where
  toJSON = toJSONText

instance FromJSON CompressionFormat where
  parseJSON = parseJSONText "CompressionFormat"
