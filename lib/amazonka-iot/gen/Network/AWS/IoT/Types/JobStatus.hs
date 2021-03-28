{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.JobStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.JobStatus
  ( JobStatus
    ( JobStatus'
    , JobStatusInProgress
    , JobStatusCanceled
    , JobStatusCompleted
    , JobStatusDeletionInProgress
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

pattern JobStatusInProgress :: JobStatus
pattern JobStatusInProgress = JobStatus' "IN_PROGRESS"

pattern JobStatusCanceled :: JobStatus
pattern JobStatusCanceled = JobStatus' "CANCELED"

pattern JobStatusCompleted :: JobStatus
pattern JobStatusCompleted = JobStatus' "COMPLETED"

pattern JobStatusDeletionInProgress :: JobStatus
pattern JobStatusDeletionInProgress = JobStatus' "DELETION_IN_PROGRESS"

{-# COMPLETE 
  JobStatusInProgress,

  JobStatusCanceled,

  JobStatusCompleted,

  JobStatusDeletionInProgress,
  JobStatus'
  #-}
