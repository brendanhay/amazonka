{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.Types.ConnectionState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkSpaces.Types.ConnectionState where

import Network.AWS.Prelude

data ConnectionState
  = Connected
  | Disconnected
  | Unknown
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

instance FromText ConnectionState where
  parser =
    takeLowerText >>= \case
      "connected" -> pure Connected
      "disconnected" -> pure Disconnected
      "unknown" -> pure Unknown
      e ->
        fromTextError $
          "Failure parsing ConnectionState from value: '" <> e
            <> "'. Accepted values: connected, disconnected, unknown"

instance ToText ConnectionState where
  toText = \case
    Connected -> "CONNECTED"
    Disconnected -> "DISCONNECTED"
    Unknown -> "UNKNOWN"

instance Hashable ConnectionState

instance NFData ConnectionState

instance ToByteString ConnectionState

instance ToQuery ConnectionState

instance ToHeader ConnectionState

instance FromJSON ConnectionState where
  parseJSON = parseJSONText "ConnectionState"
