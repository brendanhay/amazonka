{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.RecordFormatType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.RecordFormatType where

import Network.AWS.Prelude

data RecordFormatType
  = CSV
  | JSON
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

instance FromText RecordFormatType where
  parser =
    takeLowerText >>= \case
      "csv" -> pure CSV
      "json" -> pure JSON
      e ->
        fromTextError $
          "Failure parsing RecordFormatType from value: '" <> e
            <> "'. Accepted values: csv, json"

instance ToText RecordFormatType where
  toText = \case
    CSV -> "CSV"
    JSON -> "JSON"

instance Hashable RecordFormatType

instance NFData RecordFormatType

instance ToByteString RecordFormatType

instance ToQuery RecordFormatType

instance ToHeader RecordFormatType

instance ToJSON RecordFormatType where
  toJSON = toJSONText

instance FromJSON RecordFormatType where
  parseJSON = parseJSONText "RecordFormatType"
