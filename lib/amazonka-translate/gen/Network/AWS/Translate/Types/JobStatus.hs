{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.JobStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.JobStatus
  ( JobStatus
      ( JobStatus',
        JobStatusSubmitted,
        JobStatusInProgress,
        JobStatusCompleted,
        JobStatusCompletedWithError,
        JobStatusFailed,
        JobStatusStopRequested,
        JobStatusStopped,
        fromJobStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype JobStatus = JobStatus' {fromJobStatus :: Core.Text}
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

pattern JobStatusSubmitted :: JobStatus
pattern JobStatusSubmitted = JobStatus' "SUBMITTED"

pattern JobStatusInProgress :: JobStatus
pattern JobStatusInProgress = JobStatus' "IN_PROGRESS"

pattern JobStatusCompleted :: JobStatus
pattern JobStatusCompleted = JobStatus' "COMPLETED"

pattern JobStatusCompletedWithError :: JobStatus
pattern JobStatusCompletedWithError = JobStatus' "COMPLETED_WITH_ERROR"

pattern JobStatusFailed :: JobStatus
pattern JobStatusFailed = JobStatus' "FAILED"

pattern JobStatusStopRequested :: JobStatus
pattern JobStatusStopRequested = JobStatus' "STOP_REQUESTED"

pattern JobStatusStopped :: JobStatus
pattern JobStatusStopped = JobStatus' "STOPPED"

{-# COMPLETE
  JobStatusSubmitted,
  JobStatusInProgress,
  JobStatusCompleted,
  JobStatusCompletedWithError,
  JobStatusFailed,
  JobStatusStopRequested,
  JobStatusStopped,
  JobStatus'
  #-}
