{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.VirtualInterfaceState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.VirtualInterfaceState where

import Network.AWS.Prelude

data VirtualInterfaceState
  = VISAvailable
  | VISConfirming
  | VISDeleted
  | VISDeleting
  | VISDown
  | VISPending
  | VISRejected
  | VISUnknown
  | VISVerifying
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

instance FromText VirtualInterfaceState where
  parser =
    takeLowerText >>= \case
      "available" -> pure VISAvailable
      "confirming" -> pure VISConfirming
      "deleted" -> pure VISDeleted
      "deleting" -> pure VISDeleting
      "down" -> pure VISDown
      "pending" -> pure VISPending
      "rejected" -> pure VISRejected
      "unknown" -> pure VISUnknown
      "verifying" -> pure VISVerifying
      e ->
        fromTextError $
          "Failure parsing VirtualInterfaceState from value: '" <> e
            <> "'. Accepted values: available, confirming, deleted, deleting, down, pending, rejected, unknown, verifying"

instance ToText VirtualInterfaceState where
  toText = \case
    VISAvailable -> "available"
    VISConfirming -> "confirming"
    VISDeleted -> "deleted"
    VISDeleting -> "deleting"
    VISDown -> "down"
    VISPending -> "pending"
    VISRejected -> "rejected"
    VISUnknown -> "unknown"
    VISVerifying -> "verifying"

instance Hashable VirtualInterfaceState

instance NFData VirtualInterfaceState

instance ToByteString VirtualInterfaceState

instance ToQuery VirtualInterfaceState

instance ToHeader VirtualInterfaceState

instance FromJSON VirtualInterfaceState where
  parseJSON = parseJSONText "VirtualInterfaceState"
