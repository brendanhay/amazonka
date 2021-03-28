{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ResourceStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.ResourceStatus
  ( ResourceStatus
    ( ResourceStatus'
    , ResourceStatusCreateInProgress
    , ResourceStatusCreateFailed
    , ResourceStatusCreateComplete
    , ResourceStatusDeleteInProgress
    , ResourceStatusDeleteFailed
    , ResourceStatusDeleteComplete
    , ResourceStatusDeleteSkipped
    , ResourceStatusUpdateInProgress
    , ResourceStatusUpdateFailed
    , ResourceStatusUpdateComplete
    , ResourceStatusImportFailed
    , ResourceStatusImportComplete
    , ResourceStatusImportInProgress
    , ResourceStatusImportRollbackInProgress
    , ResourceStatusImportRollbackFailed
    , ResourceStatusImportRollbackComplete
    , fromResourceStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ResourceStatus = ResourceStatus'{fromResourceStatus ::
                                         Core.Text}
                           deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                           Core.Generic)
                           deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                             Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                             Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                             Core.FromText, Core.ToByteString, Core.ToQuery,
                                             Core.ToHeader)

pattern ResourceStatusCreateInProgress :: ResourceStatus
pattern ResourceStatusCreateInProgress = ResourceStatus' "CREATE_IN_PROGRESS"

pattern ResourceStatusCreateFailed :: ResourceStatus
pattern ResourceStatusCreateFailed = ResourceStatus' "CREATE_FAILED"

pattern ResourceStatusCreateComplete :: ResourceStatus
pattern ResourceStatusCreateComplete = ResourceStatus' "CREATE_COMPLETE"

pattern ResourceStatusDeleteInProgress :: ResourceStatus
pattern ResourceStatusDeleteInProgress = ResourceStatus' "DELETE_IN_PROGRESS"

pattern ResourceStatusDeleteFailed :: ResourceStatus
pattern ResourceStatusDeleteFailed = ResourceStatus' "DELETE_FAILED"

pattern ResourceStatusDeleteComplete :: ResourceStatus
pattern ResourceStatusDeleteComplete = ResourceStatus' "DELETE_COMPLETE"

pattern ResourceStatusDeleteSkipped :: ResourceStatus
pattern ResourceStatusDeleteSkipped = ResourceStatus' "DELETE_SKIPPED"

pattern ResourceStatusUpdateInProgress :: ResourceStatus
pattern ResourceStatusUpdateInProgress = ResourceStatus' "UPDATE_IN_PROGRESS"

pattern ResourceStatusUpdateFailed :: ResourceStatus
pattern ResourceStatusUpdateFailed = ResourceStatus' "UPDATE_FAILED"

pattern ResourceStatusUpdateComplete :: ResourceStatus
pattern ResourceStatusUpdateComplete = ResourceStatus' "UPDATE_COMPLETE"

pattern ResourceStatusImportFailed :: ResourceStatus
pattern ResourceStatusImportFailed = ResourceStatus' "IMPORT_FAILED"

pattern ResourceStatusImportComplete :: ResourceStatus
pattern ResourceStatusImportComplete = ResourceStatus' "IMPORT_COMPLETE"

pattern ResourceStatusImportInProgress :: ResourceStatus
pattern ResourceStatusImportInProgress = ResourceStatus' "IMPORT_IN_PROGRESS"

pattern ResourceStatusImportRollbackInProgress :: ResourceStatus
pattern ResourceStatusImportRollbackInProgress = ResourceStatus' "IMPORT_ROLLBACK_IN_PROGRESS"

pattern ResourceStatusImportRollbackFailed :: ResourceStatus
pattern ResourceStatusImportRollbackFailed = ResourceStatus' "IMPORT_ROLLBACK_FAILED"

pattern ResourceStatusImportRollbackComplete :: ResourceStatus
pattern ResourceStatusImportRollbackComplete = ResourceStatus' "IMPORT_ROLLBACK_COMPLETE"

{-# COMPLETE 
  ResourceStatusCreateInProgress,

  ResourceStatusCreateFailed,

  ResourceStatusCreateComplete,

  ResourceStatusDeleteInProgress,

  ResourceStatusDeleteFailed,

  ResourceStatusDeleteComplete,

  ResourceStatusDeleteSkipped,

  ResourceStatusUpdateInProgress,

  ResourceStatusUpdateFailed,

  ResourceStatusUpdateComplete,

  ResourceStatusImportFailed,

  ResourceStatusImportComplete,

  ResourceStatusImportInProgress,

  ResourceStatusImportRollbackInProgress,

  ResourceStatusImportRollbackFailed,

  ResourceStatusImportRollbackComplete,
  ResourceStatus'
  #-}
