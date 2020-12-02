{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.WorkflowRunStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.WorkflowRunStatus where

import Network.AWS.Prelude

data WorkflowRunStatus
  = WRSCompleted
  | WRSError'
  | WRSRunning
  | WRSStopped
  | WRSStopping
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

instance FromText WorkflowRunStatus where
  parser =
    takeLowerText >>= \case
      "completed" -> pure WRSCompleted
      "error" -> pure WRSError'
      "running" -> pure WRSRunning
      "stopped" -> pure WRSStopped
      "stopping" -> pure WRSStopping
      e ->
        fromTextError $
          "Failure parsing WorkflowRunStatus from value: '" <> e
            <> "'. Accepted values: completed, error, running, stopped, stopping"

instance ToText WorkflowRunStatus where
  toText = \case
    WRSCompleted -> "COMPLETED"
    WRSError' -> "ERROR"
    WRSRunning -> "RUNNING"
    WRSStopped -> "STOPPED"
    WRSStopping -> "STOPPING"

instance Hashable WorkflowRunStatus

instance NFData WorkflowRunStatus

instance ToByteString WorkflowRunStatus

instance ToQuery WorkflowRunStatus

instance ToHeader WorkflowRunStatus

instance FromJSON WorkflowRunStatus where
  parseJSON = parseJSONText "WorkflowRunStatus"
