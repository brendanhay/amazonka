{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuditMitigationActionsExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditMitigationActionsExecutionStatus where

import Network.AWS.Prelude

data AuditMitigationActionsExecutionStatus
  = AMAESCanceled
  | AMAESCompleted
  | AMAESFailed
  | AMAESInProgress
  | AMAESPending
  | AMAESSkipped
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

instance FromText AuditMitigationActionsExecutionStatus where
  parser =
    takeLowerText >>= \case
      "canceled" -> pure AMAESCanceled
      "completed" -> pure AMAESCompleted
      "failed" -> pure AMAESFailed
      "in_progress" -> pure AMAESInProgress
      "pending" -> pure AMAESPending
      "skipped" -> pure AMAESSkipped
      e ->
        fromTextError $
          "Failure parsing AuditMitigationActionsExecutionStatus from value: '" <> e
            <> "'. Accepted values: canceled, completed, failed, in_progress, pending, skipped"

instance ToText AuditMitigationActionsExecutionStatus where
  toText = \case
    AMAESCanceled -> "CANCELED"
    AMAESCompleted -> "COMPLETED"
    AMAESFailed -> "FAILED"
    AMAESInProgress -> "IN_PROGRESS"
    AMAESPending -> "PENDING"
    AMAESSkipped -> "SKIPPED"

instance Hashable AuditMitigationActionsExecutionStatus

instance NFData AuditMitigationActionsExecutionStatus

instance ToByteString AuditMitigationActionsExecutionStatus

instance ToQuery AuditMitigationActionsExecutionStatus

instance ToHeader AuditMitigationActionsExecutionStatus

instance ToJSON AuditMitigationActionsExecutionStatus where
  toJSON = toJSONText

instance FromJSON AuditMitigationActionsExecutionStatus where
  parseJSON = parseJSONText "AuditMitigationActionsExecutionStatus"
