{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.JobStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.JobStatus
  ( JobStatus
    ( JobStatus'
    , JobStatusCreated
    , JobStatusPreparingForInitialization
    , JobStatusInitializing
    , JobStatusProcessing
    , JobStatusPendingJob
    , JobStatusCompleting
    , JobStatusCompleted
    , JobStatusFailing
    , JobStatusFailed
    , fromJobStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype JobStatus = JobStatus'{fromJobStatus :: Core.Text}
                      deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                      Core.Generic)
                      deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                        Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                        Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                        Core.FromText, Core.ToByteString, Core.ToQuery,
                                        Core.ToHeader)

pattern JobStatusCreated :: JobStatus
pattern JobStatusCreated = JobStatus' "CREATED"

pattern JobStatusPreparingForInitialization :: JobStatus
pattern JobStatusPreparingForInitialization = JobStatus' "PREPARING_FOR_INITIALIZATION"

pattern JobStatusInitializing :: JobStatus
pattern JobStatusInitializing = JobStatus' "INITIALIZING"

pattern JobStatusProcessing :: JobStatus
pattern JobStatusProcessing = JobStatus' "PROCESSING"

pattern JobStatusPendingJob :: JobStatus
pattern JobStatusPendingJob = JobStatus' "PENDING_JOB"

pattern JobStatusCompleting :: JobStatus
pattern JobStatusCompleting = JobStatus' "COMPLETING"

pattern JobStatusCompleted :: JobStatus
pattern JobStatusCompleted = JobStatus' "COMPLETED"

pattern JobStatusFailing :: JobStatus
pattern JobStatusFailing = JobStatus' "FAILING"

pattern JobStatusFailed :: JobStatus
pattern JobStatusFailed = JobStatus' "FAILED"

{-# COMPLETE 
  JobStatusCreated,

  JobStatusPreparingForInitialization,

  JobStatusInitializing,

  JobStatusProcessing,

  JobStatusPendingJob,

  JobStatusCompleting,

  JobStatusCompleted,

  JobStatusFailing,

  JobStatusFailed,
  JobStatus'
  #-}
