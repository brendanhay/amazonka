{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuditMitigationActionsExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.AuditMitigationActionsExecutionStatus
  ( AuditMitigationActionsExecutionStatus
    ( AuditMitigationActionsExecutionStatus'
    , AuditMitigationActionsExecutionStatusInProgress
    , AuditMitigationActionsExecutionStatusCompleted
    , AuditMitigationActionsExecutionStatusFailed
    , AuditMitigationActionsExecutionStatusCanceled
    , AuditMitigationActionsExecutionStatusSkipped
    , AuditMitigationActionsExecutionStatusPending
    , fromAuditMitigationActionsExecutionStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype AuditMitigationActionsExecutionStatus = AuditMitigationActionsExecutionStatus'{fromAuditMitigationActionsExecutionStatus
                                                                                       :: Core.Text}
                                                  deriving stock (Core.Eq, Core.Ord, Core.Read,
                                                                  Core.Show, Core.Generic)
                                                  deriving newtype (Core.IsString, Core.Hashable,
                                                                    Core.NFData, Core.ToJSONKey,
                                                                    Core.FromJSONKey, Core.ToJSON,
                                                                    Core.FromJSON, Core.ToXML,
                                                                    Core.FromXML, Core.ToText,
                                                                    Core.FromText,
                                                                    Core.ToByteString, Core.ToQuery,
                                                                    Core.ToHeader)

pattern AuditMitigationActionsExecutionStatusInProgress :: AuditMitigationActionsExecutionStatus
pattern AuditMitigationActionsExecutionStatusInProgress = AuditMitigationActionsExecutionStatus' "IN_PROGRESS"

pattern AuditMitigationActionsExecutionStatusCompleted :: AuditMitigationActionsExecutionStatus
pattern AuditMitigationActionsExecutionStatusCompleted = AuditMitigationActionsExecutionStatus' "COMPLETED"

pattern AuditMitigationActionsExecutionStatusFailed :: AuditMitigationActionsExecutionStatus
pattern AuditMitigationActionsExecutionStatusFailed = AuditMitigationActionsExecutionStatus' "FAILED"

pattern AuditMitigationActionsExecutionStatusCanceled :: AuditMitigationActionsExecutionStatus
pattern AuditMitigationActionsExecutionStatusCanceled = AuditMitigationActionsExecutionStatus' "CANCELED"

pattern AuditMitigationActionsExecutionStatusSkipped :: AuditMitigationActionsExecutionStatus
pattern AuditMitigationActionsExecutionStatusSkipped = AuditMitigationActionsExecutionStatus' "SKIPPED"

pattern AuditMitigationActionsExecutionStatusPending :: AuditMitigationActionsExecutionStatus
pattern AuditMitigationActionsExecutionStatusPending = AuditMitigationActionsExecutionStatus' "PENDING"

{-# COMPLETE 
  AuditMitigationActionsExecutionStatusInProgress,

  AuditMitigationActionsExecutionStatusCompleted,

  AuditMitigationActionsExecutionStatusFailed,

  AuditMitigationActionsExecutionStatusCanceled,

  AuditMitigationActionsExecutionStatusSkipped,

  AuditMitigationActionsExecutionStatusPending,
  AuditMitigationActionsExecutionStatus'
  #-}
