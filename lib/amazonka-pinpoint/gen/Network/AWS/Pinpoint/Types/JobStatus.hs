{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.JobStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.JobStatus
  ( JobStatus
      ( JobStatus',
        JSCreated,
        JSPreparingForInitialization,
        JSInitializing,
        JSProcessing,
        JSPendingJob,
        JSCompleting,
        JSCompleted,
        JSFailing,
        JSFailed
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype JobStatus = JobStatus' Lude.Text
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

pattern JSCreated :: JobStatus
pattern JSCreated = JobStatus' "CREATED"

pattern JSPreparingForInitialization :: JobStatus
pattern JSPreparingForInitialization = JobStatus' "PREPARING_FOR_INITIALIZATION"

pattern JSInitializing :: JobStatus
pattern JSInitializing = JobStatus' "INITIALIZING"

pattern JSProcessing :: JobStatus
pattern JSProcessing = JobStatus' "PROCESSING"

pattern JSPendingJob :: JobStatus
pattern JSPendingJob = JobStatus' "PENDING_JOB"

pattern JSCompleting :: JobStatus
pattern JSCompleting = JobStatus' "COMPLETING"

pattern JSCompleted :: JobStatus
pattern JSCompleted = JobStatus' "COMPLETED"

pattern JSFailing :: JobStatus
pattern JSFailing = JobStatus' "FAILING"

pattern JSFailed :: JobStatus
pattern JSFailed = JobStatus' "FAILED"

{-# COMPLETE
  JSCreated,
  JSPreparingForInitialization,
  JSInitializing,
  JSProcessing,
  JSPendingJob,
  JSCompleting,
  JSCompleted,
  JSFailing,
  JSFailed,
  JobStatus'
  #-}
