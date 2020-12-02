{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.ClusterState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.ClusterState where

import Network.AWS.Prelude

data ClusterState
  = AwaitingQuorum
  | Cancelled
  | Complete
  | InUse
  | Pending
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

instance FromText ClusterState where
  parser =
    takeLowerText >>= \case
      "awaitingquorum" -> pure AwaitingQuorum
      "cancelled" -> pure Cancelled
      "complete" -> pure Complete
      "inuse" -> pure InUse
      "pending" -> pure Pending
      e ->
        fromTextError $
          "Failure parsing ClusterState from value: '" <> e
            <> "'. Accepted values: awaitingquorum, cancelled, complete, inuse, pending"

instance ToText ClusterState where
  toText = \case
    AwaitingQuorum -> "AwaitingQuorum"
    Cancelled -> "Cancelled"
    Complete -> "Complete"
    InUse -> "InUse"
    Pending -> "Pending"

instance Hashable ClusterState

instance NFData ClusterState

instance ToByteString ClusterState

instance ToQuery ClusterState

instance ToHeader ClusterState

instance FromJSON ClusterState where
  parseJSON = parseJSONText "ClusterState"
