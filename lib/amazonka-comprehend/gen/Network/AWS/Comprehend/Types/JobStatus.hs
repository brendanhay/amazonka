-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.JobStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.JobStatus
  ( JobStatus
      ( JobStatus',
        Completed,
        Failed,
        InProgress,
        StopRequested,
        Stopped,
        Submitted
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

pattern Completed :: JobStatus
pattern Completed = JobStatus' "COMPLETED"

pattern Failed :: JobStatus
pattern Failed = JobStatus' "FAILED"

pattern InProgress :: JobStatus
pattern InProgress = JobStatus' "IN_PROGRESS"

pattern StopRequested :: JobStatus
pattern StopRequested = JobStatus' "STOP_REQUESTED"

pattern Stopped :: JobStatus
pattern Stopped = JobStatus' "STOPPED"

pattern Submitted :: JobStatus
pattern Submitted = JobStatus' "SUBMITTED"

{-# COMPLETE
  Completed,
  Failed,
  InProgress,
  StopRequested,
  Stopped,
  Submitted,
  JobStatus'
  #-}
