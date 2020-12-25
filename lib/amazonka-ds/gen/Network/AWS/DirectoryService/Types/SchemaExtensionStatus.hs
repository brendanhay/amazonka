{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.SchemaExtensionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.SchemaExtensionStatus
  ( SchemaExtensionStatus
      ( SchemaExtensionStatus',
        SchemaExtensionStatusInitializing,
        SchemaExtensionStatusCreatingSnapshot,
        SchemaExtensionStatusUpdatingSchema,
        SchemaExtensionStatusReplicating,
        SchemaExtensionStatusCancelInProgress,
        SchemaExtensionStatusRollbackInProgress,
        SchemaExtensionStatusCancelled,
        SchemaExtensionStatusFailed,
        SchemaExtensionStatusCompleted,
        fromSchemaExtensionStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype SchemaExtensionStatus = SchemaExtensionStatus'
  { fromSchemaExtensionStatus ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern SchemaExtensionStatusInitializing :: SchemaExtensionStatus
pattern SchemaExtensionStatusInitializing = SchemaExtensionStatus' "Initializing"

pattern SchemaExtensionStatusCreatingSnapshot :: SchemaExtensionStatus
pattern SchemaExtensionStatusCreatingSnapshot = SchemaExtensionStatus' "CreatingSnapshot"

pattern SchemaExtensionStatusUpdatingSchema :: SchemaExtensionStatus
pattern SchemaExtensionStatusUpdatingSchema = SchemaExtensionStatus' "UpdatingSchema"

pattern SchemaExtensionStatusReplicating :: SchemaExtensionStatus
pattern SchemaExtensionStatusReplicating = SchemaExtensionStatus' "Replicating"

pattern SchemaExtensionStatusCancelInProgress :: SchemaExtensionStatus
pattern SchemaExtensionStatusCancelInProgress = SchemaExtensionStatus' "CancelInProgress"

pattern SchemaExtensionStatusRollbackInProgress :: SchemaExtensionStatus
pattern SchemaExtensionStatusRollbackInProgress = SchemaExtensionStatus' "RollbackInProgress"

pattern SchemaExtensionStatusCancelled :: SchemaExtensionStatus
pattern SchemaExtensionStatusCancelled = SchemaExtensionStatus' "Cancelled"

pattern SchemaExtensionStatusFailed :: SchemaExtensionStatus
pattern SchemaExtensionStatusFailed = SchemaExtensionStatus' "Failed"

pattern SchemaExtensionStatusCompleted :: SchemaExtensionStatus
pattern SchemaExtensionStatusCompleted = SchemaExtensionStatus' "Completed"

{-# COMPLETE
  SchemaExtensionStatusInitializing,
  SchemaExtensionStatusCreatingSnapshot,
  SchemaExtensionStatusUpdatingSchema,
  SchemaExtensionStatusReplicating,
  SchemaExtensionStatusCancelInProgress,
  SchemaExtensionStatusRollbackInProgress,
  SchemaExtensionStatusCancelled,
  SchemaExtensionStatusFailed,
  SchemaExtensionStatusCompleted,
  SchemaExtensionStatus'
  #-}
