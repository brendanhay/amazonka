{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.NotebookExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.NotebookExecutionStatus where

import Network.AWS.Prelude

data NotebookExecutionStatus
  = NESFailed
  | NESFailing
  | NESFinished
  | NESFinishing
  | NESRunning
  | NESStartPending
  | NESStarting
  | NESStopPending
  | NESStopped
  | NESStopping
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

instance FromText NotebookExecutionStatus where
  parser =
    takeLowerText >>= \case
      "failed" -> pure NESFailed
      "failing" -> pure NESFailing
      "finished" -> pure NESFinished
      "finishing" -> pure NESFinishing
      "running" -> pure NESRunning
      "start_pending" -> pure NESStartPending
      "starting" -> pure NESStarting
      "stop_pending" -> pure NESStopPending
      "stopped" -> pure NESStopped
      "stopping" -> pure NESStopping
      e ->
        fromTextError $
          "Failure parsing NotebookExecutionStatus from value: '" <> e
            <> "'. Accepted values: failed, failing, finished, finishing, running, start_pending, starting, stop_pending, stopped, stopping"

instance ToText NotebookExecutionStatus where
  toText = \case
    NESFailed -> "FAILED"
    NESFailing -> "FAILING"
    NESFinished -> "FINISHED"
    NESFinishing -> "FINISHING"
    NESRunning -> "RUNNING"
    NESStartPending -> "START_PENDING"
    NESStarting -> "STARTING"
    NESStopPending -> "STOP_PENDING"
    NESStopped -> "STOPPED"
    NESStopping -> "STOPPING"

instance Hashable NotebookExecutionStatus

instance NFData NotebookExecutionStatus

instance ToByteString NotebookExecutionStatus

instance ToQuery NotebookExecutionStatus

instance ToHeader NotebookExecutionStatus

instance ToJSON NotebookExecutionStatus where
  toJSON = toJSONText

instance FromJSON NotebookExecutionStatus where
  parseJSON = parseJSONText "NotebookExecutionStatus"
