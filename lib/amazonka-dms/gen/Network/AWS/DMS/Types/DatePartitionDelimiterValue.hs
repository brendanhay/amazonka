{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.DatePartitionDelimiterValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.DatePartitionDelimiterValue where

import Network.AWS.Prelude

data DatePartitionDelimiterValue
  = DPDVDash
  | DPDVNone
  | DPDVSlash
  | DPDVUnderscore
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

instance FromText DatePartitionDelimiterValue where
  parser =
    takeLowerText >>= \case
      "dash" -> pure DPDVDash
      "none" -> pure DPDVNone
      "slash" -> pure DPDVSlash
      "underscore" -> pure DPDVUnderscore
      e ->
        fromTextError $
          "Failure parsing DatePartitionDelimiterValue from value: '" <> e
            <> "'. Accepted values: dash, none, slash, underscore"

instance ToText DatePartitionDelimiterValue where
  toText = \case
    DPDVDash -> "DASH"
    DPDVNone -> "NONE"
    DPDVSlash -> "SLASH"
    DPDVUnderscore -> "UNDERSCORE"

instance Hashable DatePartitionDelimiterValue

instance NFData DatePartitionDelimiterValue

instance ToByteString DatePartitionDelimiterValue

instance ToQuery DatePartitionDelimiterValue

instance ToHeader DatePartitionDelimiterValue

instance ToJSON DatePartitionDelimiterValue where
  toJSON = toJSONText

instance FromJSON DatePartitionDelimiterValue where
  parseJSON = parseJSONText "DatePartitionDelimiterValue"
