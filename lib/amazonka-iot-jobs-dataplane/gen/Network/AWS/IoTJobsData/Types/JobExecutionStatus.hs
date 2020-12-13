{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTJobsData.Types.JobExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTJobsData.Types.JobExecutionStatus
  ( JobExecutionStatus
      ( JobExecutionStatus',
        Queued,
        InProgress,
        Succeeded,
        Failed,
        TimedOut,
        Rejected,
        Removed,
        Canceled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype JobExecutionStatus = JobExecutionStatus' Lude.Text
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

pattern Queued :: JobExecutionStatus
pattern Queued = JobExecutionStatus' "QUEUED"

pattern InProgress :: JobExecutionStatus
pattern InProgress = JobExecutionStatus' "IN_PROGRESS"

pattern Succeeded :: JobExecutionStatus
pattern Succeeded = JobExecutionStatus' "SUCCEEDED"

pattern Failed :: JobExecutionStatus
pattern Failed = JobExecutionStatus' "FAILED"

pattern TimedOut :: JobExecutionStatus
pattern TimedOut = JobExecutionStatus' "TIMED_OUT"

pattern Rejected :: JobExecutionStatus
pattern Rejected = JobExecutionStatus' "REJECTED"

pattern Removed :: JobExecutionStatus
pattern Removed = JobExecutionStatus' "REMOVED"

pattern Canceled :: JobExecutionStatus
pattern Canceled = JobExecutionStatus' "CANCELED"

{-# COMPLETE
  Queued,
  InProgress,
  Succeeded,
  Failed,
  TimedOut,
  Rejected,
  Removed,
  Canceled,
  JobExecutionStatus'
  #-}
