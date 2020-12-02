{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.JobStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.JobStatus where

import Network.AWS.Prelude

data JobStatus
  = JSCompleted
  | JSCompleting
  | JSCreated
  | JSFailed
  | JSFailing
  | JSInitializing
  | JSPendingJob
  | JSPreparingForInitialization
  | JSProcessing
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
      "completing" -> pure JSCompleting
      "created" -> pure JSCreated
      "failed" -> pure JSFailed
      "failing" -> pure JSFailing
      "initializing" -> pure JSInitializing
      "pending_job" -> pure JSPendingJob
      "preparing_for_initialization" -> pure JSPreparingForInitialization
      "processing" -> pure JSProcessing
      e ->
        fromTextError $
          "Failure parsing JobStatus from value: '" <> e
            <> "'. Accepted values: completed, completing, created, failed, failing, initializing, pending_job, preparing_for_initialization, processing"

instance ToText JobStatus where
  toText = \case
    JSCompleted -> "COMPLETED"
    JSCompleting -> "COMPLETING"
    JSCreated -> "CREATED"
    JSFailed -> "FAILED"
    JSFailing -> "FAILING"
    JSInitializing -> "INITIALIZING"
    JSPendingJob -> "PENDING_JOB"
    JSPreparingForInitialization -> "PREPARING_FOR_INITIALIZATION"
    JSProcessing -> "PROCESSING"

instance Hashable JobStatus

instance NFData JobStatus

instance ToByteString JobStatus

instance ToQuery JobStatus

instance ToHeader JobStatus

instance FromJSON JobStatus where
  parseJSON = parseJSONText "JobStatus"
