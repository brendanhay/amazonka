{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ReplicationRunState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ReplicationRunState where

import Network.AWS.Prelude

data ReplicationRunState
  = RRSActive
  | RRSCompleted
  | RRSDeleted
  | RRSDeleting
  | RRSFailed
  | RRSMissed
  | RRSPending
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

instance FromText ReplicationRunState where
  parser =
    takeLowerText >>= \case
      "active" -> pure RRSActive
      "completed" -> pure RRSCompleted
      "deleted" -> pure RRSDeleted
      "deleting" -> pure RRSDeleting
      "failed" -> pure RRSFailed
      "missed" -> pure RRSMissed
      "pending" -> pure RRSPending
      e ->
        fromTextError $
          "Failure parsing ReplicationRunState from value: '" <> e
            <> "'. Accepted values: active, completed, deleted, deleting, failed, missed, pending"

instance ToText ReplicationRunState where
  toText = \case
    RRSActive -> "ACTIVE"
    RRSCompleted -> "COMPLETED"
    RRSDeleted -> "DELETED"
    RRSDeleting -> "DELETING"
    RRSFailed -> "FAILED"
    RRSMissed -> "MISSED"
    RRSPending -> "PENDING"

instance Hashable ReplicationRunState

instance NFData ReplicationRunState

instance ToByteString ReplicationRunState

instance ToQuery ReplicationRunState

instance ToHeader ReplicationRunState

instance FromJSON ReplicationRunState where
  parseJSON = parseJSONText "ReplicationRunState"
