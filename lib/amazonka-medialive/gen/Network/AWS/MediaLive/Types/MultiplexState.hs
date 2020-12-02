{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexState where

import Network.AWS.Prelude

-- | The current state of the multiplex.
data MultiplexState
  = MSCreateFailed
  | MSCreating
  | MSDeleted
  | MSDeleting
  | MSIdle
  | MSRecovering
  | MSRunning
  | MSStarting
  | MSStopping
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

instance FromText MultiplexState where
  parser =
    takeLowerText >>= \case
      "create_failed" -> pure MSCreateFailed
      "creating" -> pure MSCreating
      "deleted" -> pure MSDeleted
      "deleting" -> pure MSDeleting
      "idle" -> pure MSIdle
      "recovering" -> pure MSRecovering
      "running" -> pure MSRunning
      "starting" -> pure MSStarting
      "stopping" -> pure MSStopping
      e ->
        fromTextError $
          "Failure parsing MultiplexState from value: '" <> e
            <> "'. Accepted values: create_failed, creating, deleted, deleting, idle, recovering, running, starting, stopping"

instance ToText MultiplexState where
  toText = \case
    MSCreateFailed -> "CREATE_FAILED"
    MSCreating -> "CREATING"
    MSDeleted -> "DELETED"
    MSDeleting -> "DELETING"
    MSIdle -> "IDLE"
    MSRecovering -> "RECOVERING"
    MSRunning -> "RUNNING"
    MSStarting -> "STARTING"
    MSStopping -> "STOPPING"

instance Hashable MultiplexState

instance NFData MultiplexState

instance ToByteString MultiplexState

instance ToQuery MultiplexState

instance ToHeader MultiplexState

instance FromJSON MultiplexState where
  parseJSON = parseJSONText "MultiplexState"
