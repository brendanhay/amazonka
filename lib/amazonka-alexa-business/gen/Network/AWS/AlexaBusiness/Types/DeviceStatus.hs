{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.DeviceStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.DeviceStatus where

import Network.AWS.Prelude

data DeviceStatus
  = Deregistered
  | Failed
  | Pending
  | Ready
  | WasOffline
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

instance FromText DeviceStatus where
  parser =
    takeLowerText >>= \case
      "deregistered" -> pure Deregistered
      "failed" -> pure Failed
      "pending" -> pure Pending
      "ready" -> pure Ready
      "was_offline" -> pure WasOffline
      e ->
        fromTextError $
          "Failure parsing DeviceStatus from value: '" <> e
            <> "'. Accepted values: deregistered, failed, pending, ready, was_offline"

instance ToText DeviceStatus where
  toText = \case
    Deregistered -> "DEREGISTERED"
    Failed -> "FAILED"
    Pending -> "PENDING"
    Ready -> "READY"
    WasOffline -> "WAS_OFFLINE"

instance Hashable DeviceStatus

instance NFData DeviceStatus

instance ToByteString DeviceStatus

instance ToQuery DeviceStatus

instance ToHeader DeviceStatus

instance FromJSON DeviceStatus where
  parseJSON = parseJSONText "DeviceStatus"
