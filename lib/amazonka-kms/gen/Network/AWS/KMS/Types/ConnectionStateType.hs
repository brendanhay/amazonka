{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.ConnectionStateType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.ConnectionStateType where

import Network.AWS.Prelude

data ConnectionStateType
  = Connected
  | Connecting
  | Disconnected
  | Disconnecting
  | Failed
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

instance FromText ConnectionStateType where
  parser =
    takeLowerText >>= \case
      "connected" -> pure Connected
      "connecting" -> pure Connecting
      "disconnected" -> pure Disconnected
      "disconnecting" -> pure Disconnecting
      "failed" -> pure Failed
      e ->
        fromTextError $
          "Failure parsing ConnectionStateType from value: '" <> e
            <> "'. Accepted values: connected, connecting, disconnected, disconnecting, failed"

instance ToText ConnectionStateType where
  toText = \case
    Connected -> "CONNECTED"
    Connecting -> "CONNECTING"
    Disconnected -> "DISCONNECTED"
    Disconnecting -> "DISCONNECTING"
    Failed -> "FAILED"

instance Hashable ConnectionStateType

instance NFData ConnectionStateType

instance ToByteString ConnectionStateType

instance ToQuery ConnectionStateType

instance ToHeader ConnectionStateType

instance FromJSON ConnectionStateType where
  parseJSON = parseJSONText "ConnectionStateType"
