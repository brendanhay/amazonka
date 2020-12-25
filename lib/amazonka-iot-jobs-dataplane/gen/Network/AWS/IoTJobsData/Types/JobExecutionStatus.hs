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
        JobExecutionStatusQueued,
        JobExecutionStatusInProgress,
        JobExecutionStatusSucceeded,
        JobExecutionStatusFailed,
        JobExecutionStatusTimedOut,
        JobExecutionStatusRejected,
        JobExecutionStatusRemoved,
        JobExecutionStatusCanceled,
        fromJobExecutionStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype JobExecutionStatus = JobExecutionStatus'
  { fromJobExecutionStatus ::
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

pattern JobExecutionStatusQueued :: JobExecutionStatus
pattern JobExecutionStatusQueued = JobExecutionStatus' "QUEUED"

pattern JobExecutionStatusInProgress :: JobExecutionStatus
pattern JobExecutionStatusInProgress = JobExecutionStatus' "IN_PROGRESS"

pattern JobExecutionStatusSucceeded :: JobExecutionStatus
pattern JobExecutionStatusSucceeded = JobExecutionStatus' "SUCCEEDED"

pattern JobExecutionStatusFailed :: JobExecutionStatus
pattern JobExecutionStatusFailed = JobExecutionStatus' "FAILED"

pattern JobExecutionStatusTimedOut :: JobExecutionStatus
pattern JobExecutionStatusTimedOut = JobExecutionStatus' "TIMED_OUT"

pattern JobExecutionStatusRejected :: JobExecutionStatus
pattern JobExecutionStatusRejected = JobExecutionStatus' "REJECTED"

pattern JobExecutionStatusRemoved :: JobExecutionStatus
pattern JobExecutionStatusRemoved = JobExecutionStatus' "REMOVED"

pattern JobExecutionStatusCanceled :: JobExecutionStatus
pattern JobExecutionStatusCanceled = JobExecutionStatus' "CANCELED"

{-# COMPLETE
  JobExecutionStatusQueued,
  JobExecutionStatusInProgress,
  JobExecutionStatusSucceeded,
  JobExecutionStatusFailed,
  JobExecutionStatusTimedOut,
  JobExecutionStatusRejected,
  JobExecutionStatusRemoved,
  JobExecutionStatusCanceled,
  JobExecutionStatus'
  #-}
