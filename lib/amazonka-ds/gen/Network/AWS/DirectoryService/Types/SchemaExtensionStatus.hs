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
        SESInitializing,
        SESCreatingSnapshot,
        SESUpdatingSchema,
        SESReplicating,
        SESCancelInProgress,
        SESRollbackInProgress,
        SESCancelled,
        SESFailed,
        SESCompleted
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

pattern SESInitializing :: SchemaExtensionStatus
pattern SESInitializing = SchemaExtensionStatus' "Initializing"

pattern SESCreatingSnapshot :: SchemaExtensionStatus
pattern SESCreatingSnapshot = SchemaExtensionStatus' "CreatingSnapshot"

pattern SESUpdatingSchema :: SchemaExtensionStatus
pattern SESUpdatingSchema = SchemaExtensionStatus' "UpdatingSchema"

pattern SESReplicating :: SchemaExtensionStatus
pattern SESReplicating = SchemaExtensionStatus' "Replicating"

pattern SESCancelInProgress :: SchemaExtensionStatus
pattern SESCancelInProgress = SchemaExtensionStatus' "CancelInProgress"

pattern SESRollbackInProgress :: SchemaExtensionStatus
pattern SESRollbackInProgress = SchemaExtensionStatus' "RollbackInProgress"

pattern SESCancelled :: SchemaExtensionStatus
pattern SESCancelled = SchemaExtensionStatus' "Cancelled"

pattern SESFailed :: SchemaExtensionStatus
pattern SESFailed = SchemaExtensionStatus' "Failed"

pattern SESCompleted :: SchemaExtensionStatus
pattern SESCompleted = SchemaExtensionStatus' "Completed"

{-# COMPLETE
  SESInitializing,
  SESCreatingSnapshot,
  SESUpdatingSchema,
  SESReplicating,
  SESCancelInProgress,
  SESRollbackInProgress,
  SESCancelled,
  SESFailed,
  SESCompleted,
  SchemaExtensionStatus'
  #-}
