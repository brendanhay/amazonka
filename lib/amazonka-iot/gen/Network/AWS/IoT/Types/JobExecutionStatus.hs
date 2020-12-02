{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.JobExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.JobExecutionStatus where

import Network.AWS.Prelude

data JobExecutionStatus
  = JESCanceled
  | JESFailed
  | JESInProgress
  | JESQueued
  | JESRejected
  | JESRemoved
  | JESSucceeded
  | JESTimedOut
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

instance FromText JobExecutionStatus where
  parser =
    takeLowerText >>= \case
      "canceled" -> pure JESCanceled
      "failed" -> pure JESFailed
      "in_progress" -> pure JESInProgress
      "queued" -> pure JESQueued
      "rejected" -> pure JESRejected
      "removed" -> pure JESRemoved
      "succeeded" -> pure JESSucceeded
      "timed_out" -> pure JESTimedOut
      e ->
        fromTextError $
          "Failure parsing JobExecutionStatus from value: '" <> e
            <> "'. Accepted values: canceled, failed, in_progress, queued, rejected, removed, succeeded, timed_out"

instance ToText JobExecutionStatus where
  toText = \case
    JESCanceled -> "CANCELED"
    JESFailed -> "FAILED"
    JESInProgress -> "IN_PROGRESS"
    JESQueued -> "QUEUED"
    JESRejected -> "REJECTED"
    JESRemoved -> "REMOVED"
    JESSucceeded -> "SUCCEEDED"
    JESTimedOut -> "TIMED_OUT"

instance Hashable JobExecutionStatus

instance NFData JobExecutionStatus

instance ToByteString JobExecutionStatus

instance ToQuery JobExecutionStatus

instance ToHeader JobExecutionStatus

instance ToJSON JobExecutionStatus where
  toJSON = toJSONText

instance FromJSON JobExecutionStatus where
  parseJSON = parseJSONText "JobExecutionStatus"
