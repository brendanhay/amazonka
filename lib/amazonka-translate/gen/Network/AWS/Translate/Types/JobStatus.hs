{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.JobStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.JobStatus where

import Network.AWS.Prelude

data JobStatus
  = JSCompleted
  | JSCompletedWithError
  | JSFailed
  | JSInProgress
  | JSStopRequested
  | JSStopped
  | JSSubmitted
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

instance FromText JobStatus where
  parser =
    takeLowerText >>= \case
      "completed" -> pure JSCompleted
      "completed_with_error" -> pure JSCompletedWithError
      "failed" -> pure JSFailed
      "in_progress" -> pure JSInProgress
      "stop_requested" -> pure JSStopRequested
      "stopped" -> pure JSStopped
      "submitted" -> pure JSSubmitted
      e ->
        fromTextError $
          "Failure parsing JobStatus from value: '" <> e
            <> "'. Accepted values: completed, completed_with_error, failed, in_progress, stop_requested, stopped, submitted"

instance ToText JobStatus where
  toText = \case
    JSCompleted -> "COMPLETED"
    JSCompletedWithError -> "COMPLETED_WITH_ERROR"
    JSFailed -> "FAILED"
    JSInProgress -> "IN_PROGRESS"
    JSStopRequested -> "STOP_REQUESTED"
    JSStopped -> "STOPPED"
    JSSubmitted -> "SUBMITTED"

instance Hashable JobStatus

instance NFData JobStatus

instance ToByteString JobStatus

instance ToQuery JobStatus

instance ToHeader JobStatus

instance ToJSON JobStatus where
  toJSON = toJSONText

instance FromJSON JobStatus where
  parseJSON = parseJSONText "JobStatus"
