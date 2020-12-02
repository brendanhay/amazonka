{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ChannelState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ChannelState where

import Network.AWS.Prelude

-- | Placeholder documentation for ChannelState
data ChannelState
  = CSCreateFailed
  | CSCreating
  | CSDeleted
  | CSDeleting
  | CSIdle
  | CSRecovering
  | CSRunning
  | CSStarting
  | CSStopping
  | CSUpdateFailed
  | CSUpdating
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

instance FromText ChannelState where
  parser =
    takeLowerText >>= \case
      "create_failed" -> pure CSCreateFailed
      "creating" -> pure CSCreating
      "deleted" -> pure CSDeleted
      "deleting" -> pure CSDeleting
      "idle" -> pure CSIdle
      "recovering" -> pure CSRecovering
      "running" -> pure CSRunning
      "starting" -> pure CSStarting
      "stopping" -> pure CSStopping
      "update_failed" -> pure CSUpdateFailed
      "updating" -> pure CSUpdating
      e ->
        fromTextError $
          "Failure parsing ChannelState from value: '" <> e
            <> "'. Accepted values: create_failed, creating, deleted, deleting, idle, recovering, running, starting, stopping, update_failed, updating"

instance ToText ChannelState where
  toText = \case
    CSCreateFailed -> "CREATE_FAILED"
    CSCreating -> "CREATING"
    CSDeleted -> "DELETED"
    CSDeleting -> "DELETING"
    CSIdle -> "IDLE"
    CSRecovering -> "RECOVERING"
    CSRunning -> "RUNNING"
    CSStarting -> "STARTING"
    CSStopping -> "STOPPING"
    CSUpdateFailed -> "UPDATE_FAILED"
    CSUpdating -> "UPDATING"

instance Hashable ChannelState

instance NFData ChannelState

instance ToByteString ChannelState

instance ToQuery ChannelState

instance ToHeader ChannelState

instance FromJSON ChannelState where
  parseJSON = parseJSONText "ChannelState"
