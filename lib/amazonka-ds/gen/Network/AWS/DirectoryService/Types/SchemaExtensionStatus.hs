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
        SESCancelInProgress,
        SESCancelled,
        SESCompleted,
        SESCreatingSnapshot,
        SESFailed,
        SESInitializing,
        SESReplicating,
        SESRollbackInProgress,
        SESUpdatingSchema
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype SchemaExtensionStatus = SchemaExtensionStatus' Lude.Text
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

pattern SESCancelInProgress :: SchemaExtensionStatus
pattern SESCancelInProgress = SchemaExtensionStatus' "CancelInProgress"

pattern SESCancelled :: SchemaExtensionStatus
pattern SESCancelled = SchemaExtensionStatus' "Cancelled"

pattern SESCompleted :: SchemaExtensionStatus
pattern SESCompleted = SchemaExtensionStatus' "Completed"

pattern SESCreatingSnapshot :: SchemaExtensionStatus
pattern SESCreatingSnapshot = SchemaExtensionStatus' "CreatingSnapshot"

pattern SESFailed :: SchemaExtensionStatus
pattern SESFailed = SchemaExtensionStatus' "Failed"

pattern SESInitializing :: SchemaExtensionStatus
pattern SESInitializing = SchemaExtensionStatus' "Initializing"

pattern SESReplicating :: SchemaExtensionStatus
pattern SESReplicating = SchemaExtensionStatus' "Replicating"

pattern SESRollbackInProgress :: SchemaExtensionStatus
pattern SESRollbackInProgress = SchemaExtensionStatus' "RollbackInProgress"

pattern SESUpdatingSchema :: SchemaExtensionStatus
pattern SESUpdatingSchema = SchemaExtensionStatus' "UpdatingSchema"

{-# COMPLETE
  SESCancelInProgress,
  SESCancelled,
  SESCompleted,
  SESCreatingSnapshot,
  SESFailed,
  SESInitializing,
  SESReplicating,
  SESRollbackInProgress,
  SESUpdatingSchema,
  SchemaExtensionStatus'
  #-}
