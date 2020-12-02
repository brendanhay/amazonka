{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SMS.Types.ReplicationJobState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SMS.Types.ReplicationJobState where

import Network.AWS.Prelude

data ReplicationJobState
  = RJSActive
  | RJSCompleted
  | RJSDeleted
  | RJSDeleting
  | RJSFailed
  | RJSFailing
  | RJSPausedOnFailure
  | RJSPending
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

instance FromText ReplicationJobState where
  parser =
    takeLowerText >>= \case
      "active" -> pure RJSActive
      "completed" -> pure RJSCompleted
      "deleted" -> pure RJSDeleted
      "deleting" -> pure RJSDeleting
      "failed" -> pure RJSFailed
      "failing" -> pure RJSFailing
      "paused_on_failure" -> pure RJSPausedOnFailure
      "pending" -> pure RJSPending
      e ->
        fromTextError $
          "Failure parsing ReplicationJobState from value: '" <> e
            <> "'. Accepted values: active, completed, deleted, deleting, failed, failing, paused_on_failure, pending"

instance ToText ReplicationJobState where
  toText = \case
    RJSActive -> "ACTIVE"
    RJSCompleted -> "COMPLETED"
    RJSDeleted -> "DELETED"
    RJSDeleting -> "DELETING"
    RJSFailed -> "FAILED"
    RJSFailing -> "FAILING"
    RJSPausedOnFailure -> "PAUSED_ON_FAILURE"
    RJSPending -> "PENDING"

instance Hashable ReplicationJobState

instance NFData ReplicationJobState

instance ToByteString ReplicationJobState

instance ToQuery ReplicationJobState

instance ToHeader ReplicationJobState

instance FromJSON ReplicationJobState where
  parseJSON = parseJSONText "ReplicationJobState"
