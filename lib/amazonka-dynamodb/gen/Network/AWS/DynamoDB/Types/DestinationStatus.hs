{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.DestinationStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.DestinationStatus where

import Network.AWS.Prelude

data DestinationStatus
  = DSActive
  | DSDisabled
  | DSDisabling
  | DSEnableFailed
  | DSEnabling
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

instance FromText DestinationStatus where
  parser =
    takeLowerText >>= \case
      "active" -> pure DSActive
      "disabled" -> pure DSDisabled
      "disabling" -> pure DSDisabling
      "enable_failed" -> pure DSEnableFailed
      "enabling" -> pure DSEnabling
      e ->
        fromTextError $
          "Failure parsing DestinationStatus from value: '" <> e
            <> "'. Accepted values: active, disabled, disabling, enable_failed, enabling"

instance ToText DestinationStatus where
  toText = \case
    DSActive -> "ACTIVE"
    DSDisabled -> "DISABLED"
    DSDisabling -> "DISABLING"
    DSEnableFailed -> "ENABLE_FAILED"
    DSEnabling -> "ENABLING"

instance Hashable DestinationStatus

instance NFData DestinationStatus

instance ToByteString DestinationStatus

instance ToQuery DestinationStatus

instance ToHeader DestinationStatus

instance FromJSON DestinationStatus where
  parseJSON = parseJSONText "DestinationStatus"
