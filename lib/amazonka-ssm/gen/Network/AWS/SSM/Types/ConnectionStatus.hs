{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ConnectionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ConnectionStatus where

import Network.AWS.Prelude

data ConnectionStatus
  = Connected
  | NotConnected
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

instance FromText ConnectionStatus where
  parser =
    takeLowerText >>= \case
      "connected" -> pure Connected
      "notconnected" -> pure NotConnected
      e ->
        fromTextError $
          "Failure parsing ConnectionStatus from value: '" <> e
            <> "'. Accepted values: connected, notconnected"

instance ToText ConnectionStatus where
  toText = \case
    Connected -> "Connected"
    NotConnected -> "NotConnected"

instance Hashable ConnectionStatus

instance NFData ConnectionStatus

instance ToByteString ConnectionStatus

instance ToQuery ConnectionStatus

instance ToHeader ConnectionStatus

instance FromJSON ConnectionStatus where
  parseJSON = parseJSONText "ConnectionStatus"
