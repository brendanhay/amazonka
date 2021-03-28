{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.JobStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CodePipeline.Types.JobStatus
  ( JobStatus
    ( JobStatus'
    , JobStatusCreated
    , JobStatusQueued
    , JobStatusDispatched
    , JobStatusInProgress
    , JobStatusTimedOut
    , JobStatusSucceeded
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
pattern JobStatusCreated = JobStatus' "Created"

pattern JobStatusQueued :: JobStatus
pattern JobStatusQueued = JobStatus' "Queued"

pattern JobStatusDispatched :: JobStatus
pattern JobStatusDispatched = JobStatus' "Dispatched"

pattern JobStatusInProgress :: JobStatus
pattern JobStatusInProgress = JobStatus' "InProgress"

pattern JobStatusTimedOut :: JobStatus
pattern JobStatusTimedOut = JobStatus' "TimedOut"

pattern JobStatusSucceeded :: JobStatus
pattern JobStatusSucceeded = JobStatus' "Succeeded"

pattern JobStatusFailed :: JobStatus
pattern JobStatusFailed = JobStatus' "Failed"

{-# COMPLETE 
  JobStatusCreated,

  JobStatusQueued,

  JobStatusDispatched,

  JobStatusInProgress,

  JobStatusTimedOut,

  JobStatusSucceeded,

  JobStatusFailed,
  JobStatus'
  #-}
