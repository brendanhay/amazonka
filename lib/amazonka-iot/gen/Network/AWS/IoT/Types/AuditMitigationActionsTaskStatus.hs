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
        AMATSInProgress,
        AMATSCompleted,
        AMATSFailed,
        AMATSCanceled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AuditMitigationActionsTaskStatus = AuditMitigationActionsTaskStatus' Lude.Text
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

pattern AMATSInProgress :: AuditMitigationActionsTaskStatus
pattern AMATSInProgress = AuditMitigationActionsTaskStatus' "IN_PROGRESS"

pattern AMATSCompleted :: AuditMitigationActionsTaskStatus
pattern AMATSCompleted = AuditMitigationActionsTaskStatus' "COMPLETED"

pattern AMATSFailed :: AuditMitigationActionsTaskStatus
pattern AMATSFailed = AuditMitigationActionsTaskStatus' "FAILED"

pattern AMATSCanceled :: AuditMitigationActionsTaskStatus
pattern AMATSCanceled = AuditMitigationActionsTaskStatus' "CANCELED"

{-# COMPLETE
  AMATSInProgress,
  AMATSCompleted,
  AMATSFailed,
  AMATSCanceled,
  AuditMitigationActionsTaskStatus'
  #-}
