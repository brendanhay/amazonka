{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.JobStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.JobStatus where

import Network.AWS.Prelude

data JobStatus
  = Failed
  | Pending
  | Runnable
  | Running
  | Starting
  | Submitted
  | Succeeded
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
      "failed" -> pure Failed
      "pending" -> pure Pending
      "runnable" -> pure Runnable
      "running" -> pure Running
      "starting" -> pure Starting
      "submitted" -> pure Submitted
      "succeeded" -> pure Succeeded
      e ->
        fromTextError $
          "Failure parsing JobStatus from value: '" <> e
            <> "'. Accepted values: failed, pending, runnable, running, starting, submitted, succeeded"

instance ToText JobStatus where
  toText = \case
    Failed -> "FAILED"
    Pending -> "PENDING"
    Runnable -> "RUNNABLE"
    Running -> "RUNNING"
    Starting -> "STARTING"
    Submitted -> "SUBMITTED"
    Succeeded -> "SUCCEEDED"

instance Hashable JobStatus

instance NFData JobStatus

instance ToByteString JobStatus

instance ToQuery JobStatus

instance ToHeader JobStatus

instance ToJSON JobStatus where
  toJSON = toJSONText

instance FromJSON JobStatus where
  parseJSON = parseJSONText "JobStatus"
