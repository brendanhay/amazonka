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
        Canceled,
        Failed,
        InProgress,
        Queued,
        Rejected,
        Removed,
        Succeeded,
        TimedOut
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

pattern Canceled :: JobExecutionStatus
pattern Canceled = JobExecutionStatus' "CANCELED"

pattern Failed :: JobExecutionStatus
pattern Failed = JobExecutionStatus' "FAILED"

pattern InProgress :: JobExecutionStatus
pattern InProgress = JobExecutionStatus' "IN_PROGRESS"

pattern Queued :: JobExecutionStatus
pattern Queued = JobExecutionStatus' "QUEUED"

pattern Rejected :: JobExecutionStatus
pattern Rejected = JobExecutionStatus' "REJECTED"

pattern Removed :: JobExecutionStatus
pattern Removed = JobExecutionStatus' "REMOVED"

pattern Succeeded :: JobExecutionStatus
pattern Succeeded = JobExecutionStatus' "SUCCEEDED"

pattern TimedOut :: JobExecutionStatus
pattern TimedOut = JobExecutionStatus' "TIMED_OUT"

{-# COMPLETE
  Canceled,
  Failed,
  InProgress,
  Queued,
  Rejected,
  Removed,
  Succeeded,
  TimedOut,
  JobExecutionStatus'
  #-}
