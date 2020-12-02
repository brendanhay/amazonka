{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.JobStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.JobStatus where

import Network.AWS.Prelude

data JobStatus
  = Completed
  | Failed
  | InProgress
  | StopRequested
  | Stopped
  | Submitted
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
      "completed" -> pure Completed
      "failed" -> pure Failed
      "in_progress" -> pure InProgress
      "stop_requested" -> pure StopRequested
      "stopped" -> pure Stopped
      "submitted" -> pure Submitted
      e ->
        fromTextError $
          "Failure parsing JobStatus from value: '" <> e
            <> "'. Accepted values: completed, failed, in_progress, stop_requested, stopped, submitted"

instance ToText JobStatus where
  toText = \case
    Completed -> "COMPLETED"
    Failed -> "FAILED"
    InProgress -> "IN_PROGRESS"
    StopRequested -> "STOP_REQUESTED"
    Stopped -> "STOPPED"
    Submitted -> "SUBMITTED"

instance Hashable JobStatus

instance NFData JobStatus

instance ToByteString JobStatus

instance ToQuery JobStatus

instance ToHeader JobStatus

instance ToJSON JobStatus where
  toJSON = toJSONText

instance FromJSON JobStatus where
  parseJSON = parseJSONText "JobStatus"
