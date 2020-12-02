{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ExecutionStatus where

import Network.AWS.Prelude

data ExecutionStatus
  = ECompleted
  | ECompletedWithViolations
  | EFailed
  | EInProgress
  | EPending
  | EStopped
  | EStopping
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
      "completed" -> pure ECompleted
      "completedwithviolations" -> pure ECompletedWithViolations
      "failed" -> pure EFailed
      "inprogress" -> pure EInProgress
      "pending" -> pure EPending
      "stopped" -> pure EStopped
      "stopping" -> pure EStopping
      e ->
        fromTextError $
          "Failure parsing ExecutionStatus from value: '" <> e
            <> "'. Accepted values: completed, completedwithviolations, failed, inprogress, pending, stopped, stopping"

instance ToText ExecutionStatus where
  toText = \case
    ECompleted -> "Completed"
    ECompletedWithViolations -> "CompletedWithViolations"
    EFailed -> "Failed"
    EInProgress -> "InProgress"
    EPending -> "Pending"
    EStopped -> "Stopped"
    EStopping -> "Stopping"

instance Hashable ExecutionStatus

instance NFData ExecutionStatus

instance ToByteString ExecutionStatus

instance ToQuery ExecutionStatus

instance ToHeader ExecutionStatus

instance ToJSON ExecutionStatus where
  toJSON = toJSONText

instance FromJSON ExecutionStatus where
  parseJSON = parseJSONText "ExecutionStatus"
