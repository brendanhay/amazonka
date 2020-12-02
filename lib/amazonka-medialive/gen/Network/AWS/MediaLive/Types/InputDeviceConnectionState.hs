{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputDeviceConnectionState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDeviceConnectionState where

import Network.AWS.Prelude

-- | The state of the connection between the input device and AWS.
data InputDeviceConnectionState
  = Connected
  | Disconnected
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

instance FromText InputDeviceConnectionState where
  parser =
    takeLowerText >>= \case
      "connected" -> pure Connected
      "disconnected" -> pure Disconnected
      e ->
        fromTextError $
          "Failure parsing InputDeviceConnectionState from value: '" <> e
            <> "'. Accepted values: connected, disconnected"

instance ToText InputDeviceConnectionState where
  toText = \case
    Connected -> "CONNECTED"
    Disconnected -> "DISCONNECTED"

instance Hashable InputDeviceConnectionState

instance NFData InputDeviceConnectionState

instance ToByteString InputDeviceConnectionState

instance ToQuery InputDeviceConnectionState

instance ToHeader InputDeviceConnectionState

instance FromJSON InputDeviceConnectionState where
  parseJSON = parseJSONText "InputDeviceConnectionState"
