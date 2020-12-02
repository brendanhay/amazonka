{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.ExportTaskStatusCode
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.ExportTaskStatusCode where

import Network.AWS.Prelude

data ExportTaskStatusCode
  = ETSCCancelled
  | ETSCCompleted
  | ETSCFailed
  | ETSCPending
  | ETSCPendingCancel
  | ETSCRunning
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

instance FromText ExportTaskStatusCode where
  parser =
    takeLowerText >>= \case
      "cancelled" -> pure ETSCCancelled
      "completed" -> pure ETSCCompleted
      "failed" -> pure ETSCFailed
      "pending" -> pure ETSCPending
      "pending_cancel" -> pure ETSCPendingCancel
      "running" -> pure ETSCRunning
      e ->
        fromTextError $
          "Failure parsing ExportTaskStatusCode from value: '" <> e
            <> "'. Accepted values: cancelled, completed, failed, pending, pending_cancel, running"

instance ToText ExportTaskStatusCode where
  toText = \case
    ETSCCancelled -> "CANCELLED"
    ETSCCompleted -> "COMPLETED"
    ETSCFailed -> "FAILED"
    ETSCPending -> "PENDING"
    ETSCPendingCancel -> "PENDING_CANCEL"
    ETSCRunning -> "RUNNING"

instance Hashable ExportTaskStatusCode

instance NFData ExportTaskStatusCode

instance ToByteString ExportTaskStatusCode

instance ToQuery ExportTaskStatusCode

instance ToHeader ExportTaskStatusCode

instance ToJSON ExportTaskStatusCode where
  toJSON = toJSONText

instance FromJSON ExportTaskStatusCode where
  parseJSON = parseJSONText "ExportTaskStatusCode"
