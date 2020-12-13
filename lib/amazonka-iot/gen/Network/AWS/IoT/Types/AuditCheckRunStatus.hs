{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuditCheckRunStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuditCheckRunStatus
  ( AuditCheckRunStatus
      ( AuditCheckRunStatus',
        ACRSInProgress,
        ACRSWaitingForDataCollection,
        ACRSCanceled,
        ACRSCompletedCompliant,
        ACRSCompletedNonCompliant,
        ACRSFailed
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype AuditCheckRunStatus = AuditCheckRunStatus' Lude.Text
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

pattern ACRSInProgress :: AuditCheckRunStatus
pattern ACRSInProgress = AuditCheckRunStatus' "IN_PROGRESS"

pattern ACRSWaitingForDataCollection :: AuditCheckRunStatus
pattern ACRSWaitingForDataCollection = AuditCheckRunStatus' "WAITING_FOR_DATA_COLLECTION"

pattern ACRSCanceled :: AuditCheckRunStatus
pattern ACRSCanceled = AuditCheckRunStatus' "CANCELED"

pattern ACRSCompletedCompliant :: AuditCheckRunStatus
pattern ACRSCompletedCompliant = AuditCheckRunStatus' "COMPLETED_COMPLIANT"

pattern ACRSCompletedNonCompliant :: AuditCheckRunStatus
pattern ACRSCompletedNonCompliant = AuditCheckRunStatus' "COMPLETED_NON_COMPLIANT"

pattern ACRSFailed :: AuditCheckRunStatus
pattern ACRSFailed = AuditCheckRunStatus' "FAILED"

{-# COMPLETE
  ACRSInProgress,
  ACRSWaitingForDataCollection,
  ACRSCanceled,
  ACRSCompletedCompliant,
  ACRSCompletedNonCompliant,
  ACRSFailed,
  AuditCheckRunStatus'
  #-}
