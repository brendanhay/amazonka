{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.Types.ImageBuilderState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppStream.Types.ImageBuilderState where

import Network.AWS.Prelude

data ImageBuilderState
  = IBSDeleting
  | IBSFailed
  | IBSPending
  | IBSRebooting
  | IBSRunning
  | IBSSnapshotting
  | IBSStopped
  | IBSStopping
  | IBSUpdatingAgent
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

instance FromText ImageBuilderState where
  parser =
    takeLowerText >>= \case
      "deleting" -> pure IBSDeleting
      "failed" -> pure IBSFailed
      "pending" -> pure IBSPending
      "rebooting" -> pure IBSRebooting
      "running" -> pure IBSRunning
      "snapshotting" -> pure IBSSnapshotting
      "stopped" -> pure IBSStopped
      "stopping" -> pure IBSStopping
      "updating_agent" -> pure IBSUpdatingAgent
      e ->
        fromTextError $
          "Failure parsing ImageBuilderState from value: '" <> e
            <> "'. Accepted values: deleting, failed, pending, rebooting, running, snapshotting, stopped, stopping, updating_agent"

instance ToText ImageBuilderState where
  toText = \case
    IBSDeleting -> "DELETING"
    IBSFailed -> "FAILED"
    IBSPending -> "PENDING"
    IBSRebooting -> "REBOOTING"
    IBSRunning -> "RUNNING"
    IBSSnapshotting -> "SNAPSHOTTING"
    IBSStopped -> "STOPPED"
    IBSStopping -> "STOPPING"
    IBSUpdatingAgent -> "UPDATING_AGENT"

instance Hashable ImageBuilderState

instance NFData ImageBuilderState

instance ToByteString ImageBuilderState

instance ToQuery ImageBuilderState

instance ToHeader ImageBuilderState

instance FromJSON ImageBuilderState where
  parseJSON = parseJSONText "ImageBuilderState"
