{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.SchemaExtensionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.SchemaExtensionStatus where

import Network.AWS.Prelude

data SchemaExtensionStatus
  = SESCancelInProgress
  | SESCancelled
  | SESCompleted
  | SESCreatingSnapshot
  | SESFailed
  | SESInitializing
  | SESReplicating
  | SESRollbackInProgress
  | SESUpdatingSchema
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

instance FromText SchemaExtensionStatus where
  parser =
    takeLowerText >>= \case
      "cancelinprogress" -> pure SESCancelInProgress
      "cancelled" -> pure SESCancelled
      "completed" -> pure SESCompleted
      "creatingsnapshot" -> pure SESCreatingSnapshot
      "failed" -> pure SESFailed
      "initializing" -> pure SESInitializing
      "replicating" -> pure SESReplicating
      "rollbackinprogress" -> pure SESRollbackInProgress
      "updatingschema" -> pure SESUpdatingSchema
      e ->
        fromTextError $
          "Failure parsing SchemaExtensionStatus from value: '" <> e
            <> "'. Accepted values: cancelinprogress, cancelled, completed, creatingsnapshot, failed, initializing, replicating, rollbackinprogress, updatingschema"

instance ToText SchemaExtensionStatus where
  toText = \case
    SESCancelInProgress -> "CancelInProgress"
    SESCancelled -> "Cancelled"
    SESCompleted -> "Completed"
    SESCreatingSnapshot -> "CreatingSnapshot"
    SESFailed -> "Failed"
    SESInitializing -> "Initializing"
    SESReplicating -> "Replicating"
    SESRollbackInProgress -> "RollbackInProgress"
    SESUpdatingSchema -> "UpdatingSchema"

instance Hashable SchemaExtensionStatus

instance NFData SchemaExtensionStatus

instance ToByteString SchemaExtensionStatus

instance ToQuery SchemaExtensionStatus

instance ToHeader SchemaExtensionStatus

instance FromJSON SchemaExtensionStatus where
  parseJSON = parseJSONText "SchemaExtensionStatus"
