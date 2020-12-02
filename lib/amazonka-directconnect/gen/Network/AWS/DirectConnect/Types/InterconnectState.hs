{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.InterconnectState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.InterconnectState where

import Network.AWS.Prelude

data InterconnectState
  = ISAvailable
  | ISDeleted
  | ISDeleting
  | ISDown
  | ISPending
  | ISRequested
  | ISUnknown
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

instance FromText InterconnectState where
  parser =
    takeLowerText >>= \case
      "available" -> pure ISAvailable
      "deleted" -> pure ISDeleted
      "deleting" -> pure ISDeleting
      "down" -> pure ISDown
      "pending" -> pure ISPending
      "requested" -> pure ISRequested
      "unknown" -> pure ISUnknown
      e ->
        fromTextError $
          "Failure parsing InterconnectState from value: '" <> e
            <> "'. Accepted values: available, deleted, deleting, down, pending, requested, unknown"

instance ToText InterconnectState where
  toText = \case
    ISAvailable -> "available"
    ISDeleted -> "deleted"
    ISDeleting -> "deleting"
    ISDown -> "down"
    ISPending -> "pending"
    ISRequested -> "requested"
    ISUnknown -> "unknown"

instance Hashable InterconnectState

instance NFData InterconnectState

instance ToByteString InterconnectState

instance ToQuery InterconnectState

instance ToHeader InterconnectState

instance FromJSON InterconnectState where
  parseJSON = parseJSONText "InterconnectState"
