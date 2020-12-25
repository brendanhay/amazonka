{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuditMitigationActionsTaskStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditMitigationActionsTaskStatus
  ( AuditMitigationActionsTaskStatus
      ( AuditMitigationActionsTaskStatus',
        AuditMitigationActionsTaskStatusInProgress,
        AuditMitigationActionsTaskStatusCompleted,
        AuditMitigationActionsTaskStatusFailed,
        AuditMitigationActionsTaskStatusCanceled,
        fromAuditMitigationActionsTaskStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype AuditMitigationActionsTaskStatus = AuditMitigationActionsTaskStatus'
  { fromAuditMitigationActionsTaskStatus ::
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

pattern AuditMitigationActionsTaskStatusInProgress :: AuditMitigationActionsTaskStatus
pattern AuditMitigationActionsTaskStatusInProgress = AuditMitigationActionsTaskStatus' "IN_PROGRESS"

pattern AuditMitigationActionsTaskStatusCompleted :: AuditMitigationActionsTaskStatus
pattern AuditMitigationActionsTaskStatusCompleted = AuditMitigationActionsTaskStatus' "COMPLETED"

pattern AuditMitigationActionsTaskStatusFailed :: AuditMitigationActionsTaskStatus
pattern AuditMitigationActionsTaskStatusFailed = AuditMitigationActionsTaskStatus' "FAILED"

pattern AuditMitigationActionsTaskStatusCanceled :: AuditMitigationActionsTaskStatus
pattern AuditMitigationActionsTaskStatusCanceled = AuditMitigationActionsTaskStatus' "CANCELED"

{-# COMPLETE
  AuditMitigationActionsTaskStatusInProgress,
  AuditMitigationActionsTaskStatusCompleted,
  AuditMitigationActionsTaskStatusFailed,
  AuditMitigationActionsTaskStatusCanceled,
  AuditMitigationActionsTaskStatus'
  #-}
