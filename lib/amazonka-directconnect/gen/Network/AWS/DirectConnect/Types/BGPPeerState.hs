{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.Types.BGPPeerState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectConnect.Types.BGPPeerState where

import Network.AWS.Prelude

data BGPPeerState
  = Available
  | Deleted
  | Deleting
  | Pending
  | Verifying
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

instance FromText BGPPeerState where
  parser =
    takeLowerText >>= \case
      "available" -> pure Available
      "deleted" -> pure Deleted
      "deleting" -> pure Deleting
      "pending" -> pure Pending
      "verifying" -> pure Verifying
      e ->
        fromTextError $
          "Failure parsing BGPPeerState from value: '" <> e
            <> "'. Accepted values: available, deleted, deleting, pending, verifying"

instance ToText BGPPeerState where
  toText = \case
    Available -> "available"
    Deleted -> "deleted"
    Deleting -> "deleting"
    Pending -> "pending"
    Verifying -> "verifying"

instance Hashable BGPPeerState

instance NFData BGPPeerState

instance ToByteString BGPPeerState

instance ToQuery BGPPeerState

instance ToHeader BGPPeerState

instance FromJSON BGPPeerState where
  parseJSON = parseJSONText "BGPPeerState"
