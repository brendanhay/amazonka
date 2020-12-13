{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuditTaskStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditTaskStatus
  ( AuditTaskStatus
      ( AuditTaskStatus',
        ATSInProgress,
        ATSCompleted,
        ATSFailed,
        ATSCanceled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AuditTaskStatus = AuditTaskStatus' Lude.Text
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

pattern ATSInProgress :: AuditTaskStatus
pattern ATSInProgress = AuditTaskStatus' "IN_PROGRESS"

pattern ATSCompleted :: AuditTaskStatus
pattern ATSCompleted = AuditTaskStatus' "COMPLETED"

pattern ATSFailed :: AuditTaskStatus
pattern ATSFailed = AuditTaskStatus' "FAILED"

pattern ATSCanceled :: AuditTaskStatus
pattern ATSCanceled = AuditTaskStatus' "CANCELED"

{-# COMPLETE
  ATSInProgress,
  ATSCompleted,
  ATSFailed,
  ATSCanceled,
  AuditTaskStatus'
  #-}
