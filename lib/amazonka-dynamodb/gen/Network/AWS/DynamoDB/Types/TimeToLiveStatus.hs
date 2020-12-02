{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.TimeToLiveStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.TimeToLiveStatus where

import Network.AWS.Prelude

data TimeToLiveStatus
  = Disabled
  | Disabling
  | Enabled
  | Enabling
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

instance FromText TimeToLiveStatus where
  parser =
    takeLowerText >>= \case
      "disabled" -> pure Disabled
      "disabling" -> pure Disabling
      "enabled" -> pure Enabled
      "enabling" -> pure Enabling
      e ->
        fromTextError $
          "Failure parsing TimeToLiveStatus from value: '" <> e
            <> "'. Accepted values: disabled, disabling, enabled, enabling"

instance ToText TimeToLiveStatus where
  toText = \case
    Disabled -> "DISABLED"
    Disabling -> "DISABLING"
    Enabled -> "ENABLED"
    Enabling -> "ENABLING"

instance Hashable TimeToLiveStatus

instance NFData TimeToLiveStatus

instance ToByteString TimeToLiveStatus

instance ToQuery TimeToLiveStatus

instance ToHeader TimeToLiveStatus

instance FromJSON TimeToLiveStatus where
  parseJSON = parseJSONText "TimeToLiveStatus"
