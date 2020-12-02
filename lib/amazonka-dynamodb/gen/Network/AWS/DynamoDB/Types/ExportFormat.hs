{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.ExportFormat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.ExportFormat where

import Network.AWS.Prelude

data ExportFormat
  = DynamodbJSON
  | Ion
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

instance FromText ExportFormat where
  parser =
    takeLowerText >>= \case
      "dynamodb_json" -> pure DynamodbJSON
      "ion" -> pure Ion
      e ->
        fromTextError $
          "Failure parsing ExportFormat from value: '" <> e
            <> "'. Accepted values: dynamodb_json, ion"

instance ToText ExportFormat where
  toText = \case
    DynamodbJSON -> "DYNAMODB_JSON"
    Ion -> "ION"

instance Hashable ExportFormat

instance NFData ExportFormat

instance ToByteString ExportFormat

instance ToQuery ExportFormat

instance ToHeader ExportFormat

instance ToJSON ExportFormat where
  toJSON = toJSONText

instance FromJSON ExportFormat where
  parseJSON = parseJSONText "ExportFormat"
