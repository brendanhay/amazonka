{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingJobStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingJobStatus
  ( ProcessingJobStatus
      ( ProcessingJobStatus',
        ProcessingJobStatusInProgress,
        ProcessingJobStatusCompleted,
        ProcessingJobStatusFailed,
        ProcessingJobStatusStopping,
        ProcessingJobStatusStopped,
        fromProcessingJobStatus
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ProcessingJobStatus = ProcessingJobStatus'
  { fromProcessingJobStatus ::
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

pattern ProcessingJobStatusInProgress :: ProcessingJobStatus
pattern ProcessingJobStatusInProgress = ProcessingJobStatus' "InProgress"

pattern ProcessingJobStatusCompleted :: ProcessingJobStatus
pattern ProcessingJobStatusCompleted = ProcessingJobStatus' "Completed"

pattern ProcessingJobStatusFailed :: ProcessingJobStatus
pattern ProcessingJobStatusFailed = ProcessingJobStatus' "Failed"

pattern ProcessingJobStatusStopping :: ProcessingJobStatus
pattern ProcessingJobStatusStopping = ProcessingJobStatus' "Stopping"

pattern ProcessingJobStatusStopped :: ProcessingJobStatus
pattern ProcessingJobStatusStopped = ProcessingJobStatus' "Stopped"

{-# COMPLETE
  ProcessingJobStatusInProgress,
  ProcessingJobStatusCompleted,
  ProcessingJobStatusFailed,
  ProcessingJobStatusStopping,
  ProcessingJobStatusStopped,
  ProcessingJobStatus'
  #-}
