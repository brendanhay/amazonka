-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuditMitigationActionsExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditMitigationActionsExecutionStatus
  ( AuditMitigationActionsExecutionStatus
      ( AuditMitigationActionsExecutionStatus',
        AMAESCanceled,
        AMAESCompleted,
        AMAESFailed,
        AMAESInProgress,
        AMAESPending,
        AMAESSkipped
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AuditMitigationActionsExecutionStatus = AuditMitigationActionsExecutionStatus' Lude.Text
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

pattern AMAESCanceled :: AuditMitigationActionsExecutionStatus
pattern AMAESCanceled = AuditMitigationActionsExecutionStatus' "CANCELED"

pattern AMAESCompleted :: AuditMitigationActionsExecutionStatus
pattern AMAESCompleted = AuditMitigationActionsExecutionStatus' "COMPLETED"

pattern AMAESFailed :: AuditMitigationActionsExecutionStatus
pattern AMAESFailed = AuditMitigationActionsExecutionStatus' "FAILED"

pattern AMAESInProgress :: AuditMitigationActionsExecutionStatus
pattern AMAESInProgress = AuditMitigationActionsExecutionStatus' "IN_PROGRESS"

pattern AMAESPending :: AuditMitigationActionsExecutionStatus
pattern AMAESPending = AuditMitigationActionsExecutionStatus' "PENDING"

pattern AMAESSkipped :: AuditMitigationActionsExecutionStatus
pattern AMAESSkipped = AuditMitigationActionsExecutionStatus' "SKIPPED"

{-# COMPLETE
  AMAESCanceled,
  AMAESCompleted,
  AMAESFailed,
  AMAESInProgress,
  AMAESPending,
  AMAESSkipped,
  AuditMitigationActionsExecutionStatus'
  #-}
