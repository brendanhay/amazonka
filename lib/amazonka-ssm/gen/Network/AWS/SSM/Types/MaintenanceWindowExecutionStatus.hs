{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowExecutionStatus where

import Network.AWS.Prelude

data MaintenanceWindowExecutionStatus
  = MWESCancelled
  | MWESCancelling
  | MWESFailed
  | MWESInProgress
  | MWESPending
  | MWESSkippedOverlapping
  | MWESSuccess
  | MWESTimedOut
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

instance FromText MaintenanceWindowExecutionStatus where
  parser =
    takeLowerText >>= \case
      "cancelled" -> pure MWESCancelled
      "cancelling" -> pure MWESCancelling
      "failed" -> pure MWESFailed
      "in_progress" -> pure MWESInProgress
      "pending" -> pure MWESPending
      "skipped_overlapping" -> pure MWESSkippedOverlapping
      "success" -> pure MWESSuccess
      "timed_out" -> pure MWESTimedOut
      e ->
        fromTextError $
          "Failure parsing MaintenanceWindowExecutionStatus from value: '" <> e
            <> "'. Accepted values: cancelled, cancelling, failed, in_progress, pending, skipped_overlapping, success, timed_out"

instance ToText MaintenanceWindowExecutionStatus where
  toText = \case
    MWESCancelled -> "CANCELLED"
    MWESCancelling -> "CANCELLING"
    MWESFailed -> "FAILED"
    MWESInProgress -> "IN_PROGRESS"
    MWESPending -> "PENDING"
    MWESSkippedOverlapping -> "SKIPPED_OVERLAPPING"
    MWESSuccess -> "SUCCESS"
    MWESTimedOut -> "TIMED_OUT"

instance Hashable MaintenanceWindowExecutionStatus

instance NFData MaintenanceWindowExecutionStatus

instance ToByteString MaintenanceWindowExecutionStatus

instance ToQuery MaintenanceWindowExecutionStatus

instance ToHeader MaintenanceWindowExecutionStatus

instance FromJSON MaintenanceWindowExecutionStatus where
  parseJSON = parseJSONText "MaintenanceWindowExecutionStatus"
