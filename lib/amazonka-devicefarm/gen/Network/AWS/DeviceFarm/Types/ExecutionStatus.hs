{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.ExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.ExecutionStatus where

import Network.AWS.Prelude

data ExecutionStatus
  = Completed
  | Pending
  | PendingConcurrency
  | PendingDevice
  | Preparing
  | Processing
  | Running
  | Scheduling
  | Stopping
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

instance FromText ExecutionStatus where
  parser =
    takeLowerText >>= \case
      "completed" -> pure Completed
      "pending" -> pure Pending
      "pending_concurrency" -> pure PendingConcurrency
      "pending_device" -> pure PendingDevice
      "preparing" -> pure Preparing
      "processing" -> pure Processing
      "running" -> pure Running
      "scheduling" -> pure Scheduling
      "stopping" -> pure Stopping
      e ->
        fromTextError $
          "Failure parsing ExecutionStatus from value: '" <> e
            <> "'. Accepted values: completed, pending, pending_concurrency, pending_device, preparing, processing, running, scheduling, stopping"

instance ToText ExecutionStatus where
  toText = \case
    Completed -> "COMPLETED"
    Pending -> "PENDING"
    PendingConcurrency -> "PENDING_CONCURRENCY"
    PendingDevice -> "PENDING_DEVICE"
    Preparing -> "PREPARING"
    Processing -> "PROCESSING"
    Running -> "RUNNING"
    Scheduling -> "SCHEDULING"
    Stopping -> "STOPPING"

instance Hashable ExecutionStatus

instance NFData ExecutionStatus

instance ToByteString ExecutionStatus

instance ToQuery ExecutionStatus

instance ToHeader ExecutionStatus

instance FromJSON ExecutionStatus where
  parseJSON = parseJSONText "ExecutionStatus"
