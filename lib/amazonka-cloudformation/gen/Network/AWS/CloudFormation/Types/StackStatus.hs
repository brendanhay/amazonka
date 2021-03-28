{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.StackStatus
  ( StackStatus
    ( StackStatus'
    , StackStatusCreateInProgress
    , StackStatusCreateFailed
    , StackStatusCreateComplete
    , StackStatusRollbackInProgress
    , StackStatusRollbackFailed
    , StackStatusRollbackComplete
    , StackStatusDeleteInProgress
    , StackStatusDeleteFailed
    , StackStatusDeleteComplete
    , StackStatusUpdateInProgress
    , StackStatusUpdateCompleteCleanupInProgress
    , StackStatusUpdateComplete
    , StackStatusUpdateRollbackInProgress
    , StackStatusUpdateRollbackFailed
    , StackStatusUpdateRollbackCompleteCleanupInProgress
    , StackStatusUpdateRollbackComplete
    , StackStatusReviewInProgress
    , StackStatusImportInProgress
    , StackStatusImportComplete
    , StackStatusImportRollbackInProgress
    , StackStatusImportRollbackFailed
    , StackStatusImportRollbackComplete
    , fromStackStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype StackStatus = StackStatus'{fromStackStatus :: Core.Text}
                        deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                        Core.Generic)
                        deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                          Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                          Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                          Core.FromText, Core.ToByteString, Core.ToQuery,
                                          Core.ToHeader)

pattern StackStatusCreateInProgress :: StackStatus
pattern StackStatusCreateInProgress = StackStatus' "CREATE_IN_PROGRESS"

pattern StackStatusCreateFailed :: StackStatus
pattern StackStatusCreateFailed = StackStatus' "CREATE_FAILED"

pattern StackStatusCreateComplete :: StackStatus
pattern StackStatusCreateComplete = StackStatus' "CREATE_COMPLETE"

pattern StackStatusRollbackInProgress :: StackStatus
pattern StackStatusRollbackInProgress = StackStatus' "ROLLBACK_IN_PROGRESS"

pattern StackStatusRollbackFailed :: StackStatus
pattern StackStatusRollbackFailed = StackStatus' "ROLLBACK_FAILED"

pattern StackStatusRollbackComplete :: StackStatus
pattern StackStatusRollbackComplete = StackStatus' "ROLLBACK_COMPLETE"

pattern StackStatusDeleteInProgress :: StackStatus
pattern StackStatusDeleteInProgress = StackStatus' "DELETE_IN_PROGRESS"

pattern StackStatusDeleteFailed :: StackStatus
pattern StackStatusDeleteFailed = StackStatus' "DELETE_FAILED"

pattern StackStatusDeleteComplete :: StackStatus
pattern StackStatusDeleteComplete = StackStatus' "DELETE_COMPLETE"

pattern StackStatusUpdateInProgress :: StackStatus
pattern StackStatusUpdateInProgress = StackStatus' "UPDATE_IN_PROGRESS"

pattern StackStatusUpdateCompleteCleanupInProgress :: StackStatus
pattern StackStatusUpdateCompleteCleanupInProgress = StackStatus' "UPDATE_COMPLETE_CLEANUP_IN_PROGRESS"

pattern StackStatusUpdateComplete :: StackStatus
pattern StackStatusUpdateComplete = StackStatus' "UPDATE_COMPLETE"

pattern StackStatusUpdateRollbackInProgress :: StackStatus
pattern StackStatusUpdateRollbackInProgress = StackStatus' "UPDATE_ROLLBACK_IN_PROGRESS"

pattern StackStatusUpdateRollbackFailed :: StackStatus
pattern StackStatusUpdateRollbackFailed = StackStatus' "UPDATE_ROLLBACK_FAILED"

pattern StackStatusUpdateRollbackCompleteCleanupInProgress :: StackStatus
pattern StackStatusUpdateRollbackCompleteCleanupInProgress = StackStatus' "UPDATE_ROLLBACK_COMPLETE_CLEANUP_IN_PROGRESS"

pattern StackStatusUpdateRollbackComplete :: StackStatus
pattern StackStatusUpdateRollbackComplete = StackStatus' "UPDATE_ROLLBACK_COMPLETE"

pattern StackStatusReviewInProgress :: StackStatus
pattern StackStatusReviewInProgress = StackStatus' "REVIEW_IN_PROGRESS"

pattern StackStatusImportInProgress :: StackStatus
pattern StackStatusImportInProgress = StackStatus' "IMPORT_IN_PROGRESS"

pattern StackStatusImportComplete :: StackStatus
pattern StackStatusImportComplete = StackStatus' "IMPORT_COMPLETE"

pattern StackStatusImportRollbackInProgress :: StackStatus
pattern StackStatusImportRollbackInProgress = StackStatus' "IMPORT_ROLLBACK_IN_PROGRESS"

pattern StackStatusImportRollbackFailed :: StackStatus
pattern StackStatusImportRollbackFailed = StackStatus' "IMPORT_ROLLBACK_FAILED"

pattern StackStatusImportRollbackComplete :: StackStatus
pattern StackStatusImportRollbackComplete = StackStatus' "IMPORT_ROLLBACK_COMPLETE"

{-# COMPLETE 
  StackStatusCreateInProgress,

  StackStatusCreateFailed,

  StackStatusCreateComplete,

  StackStatusRollbackInProgress,

  StackStatusRollbackFailed,

  StackStatusRollbackComplete,

  StackStatusDeleteInProgress,

  StackStatusDeleteFailed,

  StackStatusDeleteComplete,

  StackStatusUpdateInProgress,

  StackStatusUpdateCompleteCleanupInProgress,

  StackStatusUpdateComplete,

  StackStatusUpdateRollbackInProgress,

  StackStatusUpdateRollbackFailed,

  StackStatusUpdateRollbackCompleteCleanupInProgress,

  StackStatusUpdateRollbackComplete,

  StackStatusReviewInProgress,

  StackStatusImportInProgress,

  StackStatusImportComplete,

  StackStatusImportRollbackInProgress,

  StackStatusImportRollbackFailed,

  StackStatusImportRollbackComplete,
  StackStatus'
  #-}
