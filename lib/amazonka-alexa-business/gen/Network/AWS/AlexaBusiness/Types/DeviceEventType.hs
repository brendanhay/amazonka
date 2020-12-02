{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.DeviceEventType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.DeviceEventType where

import Network.AWS.Prelude

data DeviceEventType
  = ConnectionStatus
  | DeviceStatus
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

instance FromText DeviceEventType where
  parser =
    takeLowerText >>= \case
      "connection_status" -> pure ConnectionStatus
      "device_status" -> pure DeviceStatus
      e ->
        fromTextError $
          "Failure parsing DeviceEventType from value: '" <> e
            <> "'. Accepted values: connection_status, device_status"

instance ToText DeviceEventType where
  toText = \case
    ConnectionStatus -> "CONNECTION_STATUS"
    DeviceStatus -> "DEVICE_STATUS"

instance Hashable DeviceEventType

instance NFData DeviceEventType

instance ToByteString DeviceEventType

instance ToQuery DeviceEventType

instance ToHeader DeviceEventType

instance ToJSON DeviceEventType where
  toJSON = toJSONText

instance FromJSON DeviceEventType where
  parseJSON = parseJSONText "DeviceEventType"
