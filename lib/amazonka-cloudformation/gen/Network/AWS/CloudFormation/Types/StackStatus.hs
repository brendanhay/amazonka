-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackStatus
  ( StackStatus
      ( StackStatus',
        SSCreateComplete,
        SSCreateFailed,
        SSCreateInProgress,
        SSDeleteComplete,
        SSDeleteFailed,
        SSDeleteInProgress,
        SSImportComplete,
        SSImportInProgress,
        SSImportRollbackComplete,
        SSImportRollbackFailed,
        SSImportRollbackInProgress,
        SSReviewInProgress,
        SSRollbackComplete,
        SSRollbackFailed,
        SSRollbackInProgress,
        SSUpdateComplete,
        SSUpdateCompleteCleanupInProgress,
        SSUpdateInProgress,
        SSUpdateRollbackComplete,
        SSUpdateRollbackCompleteCleanupInProgress,
        SSUpdateRollbackFailed,
        SSUpdateRollbackInProgress
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype StackStatus = StackStatus' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern SSCreateComplete :: StackStatus
pattern SSCreateComplete = StackStatus' "CREATE_COMPLETE"

pattern SSCreateFailed :: StackStatus
pattern SSCreateFailed = StackStatus' "CREATE_FAILED"

pattern SSCreateInProgress :: StackStatus
pattern SSCreateInProgress = StackStatus' "CREATE_IN_PROGRESS"

pattern SSDeleteComplete :: StackStatus
pattern SSDeleteComplete = StackStatus' "DELETE_COMPLETE"

pattern SSDeleteFailed :: StackStatus
pattern SSDeleteFailed = StackStatus' "DELETE_FAILED"

pattern SSDeleteInProgress :: StackStatus
pattern SSDeleteInProgress = StackStatus' "DELETE_IN_PROGRESS"

pattern SSImportComplete :: StackStatus
pattern SSImportComplete = StackStatus' "IMPORT_COMPLETE"

pattern SSImportInProgress :: StackStatus
pattern SSImportInProgress = StackStatus' "IMPORT_IN_PROGRESS"

pattern SSImportRollbackComplete :: StackStatus
pattern SSImportRollbackComplete = StackStatus' "IMPORT_ROLLBACK_COMPLETE"

pattern SSImportRollbackFailed :: StackStatus
pattern SSImportRollbackFailed = StackStatus' "IMPORT_ROLLBACK_FAILED"

pattern SSImportRollbackInProgress :: StackStatus
pattern SSImportRollbackInProgress = StackStatus' "IMPORT_ROLLBACK_IN_PROGRESS"

pattern SSReviewInProgress :: StackStatus
pattern SSReviewInProgress = StackStatus' "REVIEW_IN_PROGRESS"

pattern SSRollbackComplete :: StackStatus
pattern SSRollbackComplete = StackStatus' "ROLLBACK_COMPLETE"

pattern SSRollbackFailed :: StackStatus
pattern SSRollbackFailed = StackStatus' "ROLLBACK_FAILED"

pattern SSRollbackInProgress :: StackStatus
pattern SSRollbackInProgress = StackStatus' "ROLLBACK_IN_PROGRESS"

pattern SSUpdateComplete :: StackStatus
pattern SSUpdateComplete = StackStatus' "UPDATE_COMPLETE"

pattern SSUpdateCompleteCleanupInProgress :: StackStatus
pattern SSUpdateCompleteCleanupInProgress = StackStatus' "UPDATE_COMPLETE_CLEANUP_IN_PROGRESS"

pattern SSUpdateInProgress :: StackStatus
pattern SSUpdateInProgress = StackStatus' "UPDATE_IN_PROGRESS"

pattern SSUpdateRollbackComplete :: StackStatus
pattern SSUpdateRollbackComplete = StackStatus' "UPDATE_ROLLBACK_COMPLETE"

pattern SSUpdateRollbackCompleteCleanupInProgress :: StackStatus
pattern SSUpdateRollbackCompleteCleanupInProgress = StackStatus' "UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS"

pattern SSUpdateRollbackFailed :: StackStatus
pattern SSUpdateRollbackFailed = StackStatus' "UPDATE_ROLLBACK_FAILED"

pattern SSUpdateRollbackInProgress :: StackStatus
pattern SSUpdateRollbackInProgress = StackStatus' "UPDATE_ROLLBACK_IN_PROGRESS"

{-# COMPLETE
  SSCreateComplete,
  SSCreateFailed,
  SSCreateInProgress,
  SSDeleteComplete,
  SSDeleteFailed,
  SSDeleteInProgress,
  SSImportComplete,
  SSImportInProgress,
  SSImportRollbackComplete,
  SSImportRollbackFailed,
  SSImportRollbackInProgress,
  SSReviewInProgress,
  SSRollbackComplete,
  SSRollbackFailed,
  SSRollbackInProgress,
  SSUpdateComplete,
  SSUpdateCompleteCleanupInProgress,
  SSUpdateInProgress,
  SSUpdateRollbackComplete,
  SSUpdateRollbackCompleteCleanupInProgress,
  SSUpdateRollbackFailed,
  SSUpdateRollbackInProgress,
  StackStatus'
  #-}
