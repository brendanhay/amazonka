{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.SessionConnectionState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.SessionConnectionState where

import Network.AWS.Prelude

data SessionConnectionState
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

instance FromText SessionConnectionState where
  parser =
    takeLowerText >>= \case
      "connected" -> pure Connected
      "not_connected" -> pure NotConnected
      e ->
        fromTextError $
          "Failure parsing SessionConnectionState from value: '" <> e
            <> "'. Accepted values: connected, not_connected"

instance ToText SessionConnectionState where
  toText = \case
    Connected -> "CONNECTED"
    NotConnected -> "NOT_CONNECTED"

instance Hashable SessionConnectionState

instance NFData SessionConnectionState

instance ToByteString SessionConnectionState

instance ToQuery SessionConnectionState

instance ToHeader SessionConnectionState

instance FromJSON SessionConnectionState where
  parseJSON = parseJSONText "SessionConnectionState"
