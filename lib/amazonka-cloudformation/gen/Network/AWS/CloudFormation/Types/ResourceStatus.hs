{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ResourceStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ResourceStatus where

import Network.AWS.Prelude

data ResourceStatus
  = CreateComplete
  | CreateFailed
  | CreateInProgress
  | DeleteComplete
  | DeleteFailed
  | DeleteInProgress
  | DeleteSkipped
  | ImportComplete
  | ImportFailed
  | ImportInProgress
  | ImportRollbackComplete
  | ImportRollbackFailed
  | ImportRollbackInProgress
  | UpdateComplete
  | UpdateFailed
  | UpdateInProgress
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

instance FromText ResourceStatus where
  parser =
    takeLowerText >>= \case
      "create_complete" -> pure CreateComplete
      "create_failed" -> pure CreateFailed
      "create_in_progress" -> pure CreateInProgress
      "delete_complete" -> pure DeleteComplete
      "delete_failed" -> pure DeleteFailed
      "delete_in_progress" -> pure DeleteInProgress
      "delete_skipped" -> pure DeleteSkipped
      "import_complete" -> pure ImportComplete
      "import_failed" -> pure ImportFailed
      "import_in_progress" -> pure ImportInProgress
      "import_rollback_complete" -> pure ImportRollbackComplete
      "import_rollback_failed" -> pure ImportRollbackFailed
      "import_rollback_in_progress" -> pure ImportRollbackInProgress
      "update_complete" -> pure UpdateComplete
      "update_failed" -> pure UpdateFailed
      "update_in_progress" -> pure UpdateInProgress
      e ->
        fromTextError $
          "Failure parsing ResourceStatus from value: '" <> e
            <> "'. Accepted values: create_complete, create_failed, create_in_progress, delete_complete, delete_failed, delete_in_progress, delete_skipped, import_complete, import_failed, import_in_progress, import_rollback_complete, import_rollback_failed, import_rollback_in_progress, update_complete, update_failed, update_in_progress"

instance ToText ResourceStatus where
  toText = \case
    CreateComplete -> "CREATE_COMPLETE"
    CreateFailed -> "CREATE_FAILED"
    CreateInProgress -> "CREATE_IN_PROGRESS"
    DeleteComplete -> "DELETE_COMPLETE"
    DeleteFailed -> "DELETE_FAILED"
    DeleteInProgress -> "DELETE_IN_PROGRESS"
    DeleteSkipped -> "DELETE_SKIPPED"
    ImportComplete -> "IMPORT_COMPLETE"
    ImportFailed -> "IMPORT_FAILED"
    ImportInProgress -> "IMPORT_IN_PROGRESS"
    ImportRollbackComplete -> "IMPORT_ROLLBACK_COMPLETE"
    ImportRollbackFailed -> "IMPORT_ROLLBACK_FAILED"
    ImportRollbackInProgress -> "IMPORT_ROLLBACK_IN_PROGRESS"
    UpdateComplete -> "UPDATE_COMPLETE"
    UpdateFailed -> "UPDATE_FAILED"
    UpdateInProgress -> "UPDATE_IN_PROGRESS"

instance Hashable ResourceStatus

instance NFData ResourceStatus

instance ToByteString ResourceStatus

instance ToQuery ResourceStatus

instance ToHeader ResourceStatus

instance FromXML ResourceStatus where
  parseXML = parseXMLText "ResourceStatus"
