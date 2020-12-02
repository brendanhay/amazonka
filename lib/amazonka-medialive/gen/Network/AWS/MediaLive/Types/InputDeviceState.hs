{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputDeviceState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDeviceState where

import Network.AWS.Prelude

-- | The state of the input device.
data InputDeviceState
  = Idle
  | Streaming
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

instance FromText InputDeviceState where
  parser =
    takeLowerText >>= \case
      "idle" -> pure Idle
      "streaming" -> pure Streaming
      e ->
        fromTextError $
          "Failure parsing InputDeviceState from value: '" <> e
            <> "'. Accepted values: idle, streaming"

instance ToText InputDeviceState where
  toText = \case
    Idle -> "IDLE"
    Streaming -> "STREAMING"

instance Hashable InputDeviceState

instance NFData InputDeviceState

instance ToByteString InputDeviceState

instance ToQuery InputDeviceState

instance ToHeader InputDeviceState

instance FromJSON InputDeviceState where
  parseJSON = parseJSONText "InputDeviceState"
