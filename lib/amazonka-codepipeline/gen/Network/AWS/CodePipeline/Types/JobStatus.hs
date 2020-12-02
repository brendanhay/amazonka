{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.JobStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.JobStatus where

import Network.AWS.Prelude

data JobStatus
  = JSCreated
  | JSDispatched
  | JSFailed
  | JSInProgress
  | JSQueued
  | JSSucceeded
  | JSTimedOut
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
      "created" -> pure JSCreated
      "dispatched" -> pure JSDispatched
      "failed" -> pure JSFailed
      "inprogress" -> pure JSInProgress
      "queued" -> pure JSQueued
      "succeeded" -> pure JSSucceeded
      "timedout" -> pure JSTimedOut
      e ->
        fromTextError $
          "Failure parsing JobStatus from value: '" <> e
            <> "'. Accepted values: created, dispatched, failed, inprogress, queued, succeeded, timedout"

instance ToText JobStatus where
  toText = \case
    JSCreated -> "Created"
    JSDispatched -> "Dispatched"
    JSFailed -> "Failed"
    JSInProgress -> "InProgress"
    JSQueued -> "Queued"
    JSSucceeded -> "Succeeded"
    JSTimedOut -> "TimedOut"

instance Hashable JobStatus

instance NFData JobStatus

instance ToByteString JobStatus

instance ToQuery JobStatus

instance ToHeader JobStatus

instance FromJSON JobStatus where
  parseJSON = parseJSONText "JobStatus"
