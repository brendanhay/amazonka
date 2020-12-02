{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackStatus where

import Network.AWS.Prelude

data StackStatus
  = SSCreateComplete
  | SSCreateFailed
  | SSCreateInProgress
  | SSDeleteComplete
  | SSDeleteFailed
  | SSDeleteInProgress
  | SSImportComplete
  | SSImportInProgress
  | SSImportRollbackComplete
  | SSImportRollbackFailed
  | SSImportRollbackInProgress
  | SSReviewInProgress
  | SSRollbackComplete
  | SSRollbackFailed
  | SSRollbackInProgress
  | SSUpdateComplete
  | SSUpdateCompleteCleanupInProgress
  | SSUpdateInProgress
  | SSUpdateRollbackComplete
  | SSUpdateRollbackCompleteCleanupInProgress
  | SSUpdateRollbackFailed
  | SSUpdateRollbackInProgress
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

instance FromText StackStatus where
  parser =
    takeLowerText >>= \case
      "create_complete" -> pure SSCreateComplete
      "create_failed" -> pure SSCreateFailed
      "create_in_progress" -> pure SSCreateInProgress
      "delete_complete" -> pure SSDeleteComplete
      "delete_failed" -> pure SSDeleteFailed
      "delete_in_progress" -> pure SSDeleteInProgress
      "import_complete" -> pure SSImportComplete
      "import_in_progress" -> pure SSImportInProgress
      "import_rollback_complete" -> pure SSImportRollbackComplete
      "import_rollback_failed" -> pure SSImportRollbackFailed
      "import_rollback_in_progress" -> pure SSImportRollbackInProgress
      "review_in_progress" -> pure SSReviewInProgress
      "rollback_complete" -> pure SSRollbackComplete
      "rollback_failed" -> pure SSRollbackFailed
      "rollback_in_progress" -> pure SSRollbackInProgress
      "update_complete" -> pure SSUpdateComplete
      "update_complete_cleanup_in_progress" -> pure SSUpdateCompleteCleanupInProgress
      "update_in_progress" -> pure SSUpdateInProgress
      "update_rollback_complete" -> pure SSUpdateRollbackComplete
      "update_rollback_complete_cleanup_in_progress" -> pure SSUpdateRollbackCompleteCleanupInProgress
      "update_rollback_failed" -> pure SSUpdateRollbackFailed
      "update_rollback_in_progress" -> pure SSUpdateRollbackInProgress
      e ->
        fromTextError $
          "Failure parsing StackStatus from value: '" <> e
            <> "'. Accepted values: create_complete, create_failed, create_in_progress, delete_complete, delete_failed, delete_in_progress, import_complete, import_in_progress, import_rollback_complete, import_rollback_failed, import_rollback_in_progress, review_in_progress, rollback_complete, rollback_failed, rollback_in_progress, update_complete, update_complete_cleanup_in_progress, update_in_progress, update_rollback_complete, update_rollback_complete_cleanup_in_progress, update_rollback_failed, update_rollback_in_progress"

instance ToText StackStatus where
  toText = \case
    SSCreateComplete -> "CREATE_COMPLETE"
    SSCreateFailed -> "CREATE_FAILED"
    SSCreateInProgress -> "CREATE_IN_PROGRESS"
    SSDeleteComplete -> "DELETE_COMPLETE"
    SSDeleteFailed -> "DELETE_FAILED"
    SSDeleteInProgress -> "DELETE_IN_PROGRESS"
    SSImportComplete -> "IMPORT_COMPLETE"
    SSImportInProgress -> "IMPORT_IN_PROGRESS"
    SSImportRollbackComplete -> "IMPORT_ROLLBACK_COMPLETE"
    SSImportRollbackFailed -> "IMPORT_ROLLBACK_FAILED"
    SSImportRollbackInProgress -> "IMPORT_ROLLBACK_IN_PROGRESS"
    SSReviewInProgress -> "REVIEW_IN_PROGRESS"
    SSRollbackComplete -> "ROLLBACK_COMPLETE"
    SSRollbackFailed -> "ROLLBACK_FAILED"
    SSRollbackInProgress -> "ROLLBACK_IN_PROGRESS"
    SSUpdateComplete -> "UPDATE_COMPLETE"
    SSUpdateCompleteCleanupInProgress -> "UPDATE_COMPLETE_CLEANUP_IN_PROGRESS"
    SSUpdateInProgress -> "UPDATE_IN_PROGRESS"
    SSUpdateRollbackComplete -> "UPDATE_ROLLBACK_COMPLETE"
    SSUpdateRollbackCompleteCleanupInProgress -> "UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS"
    SSUpdateRollbackFailed -> "UPDATE_ROLLBACK_FAILED"
    SSUpdateRollbackInProgress -> "UPDATE_ROLLBACK_IN_PROGRESS"

instance Hashable StackStatus

instance NFData StackStatus

instance ToByteString StackStatus

instance ToQuery StackStatus

instance ToHeader StackStatus

instance FromXML StackStatus where
  parseXML = parseXMLText "StackStatus"
