{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.JobExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.JobExecutionStatus
  ( JobExecutionStatus
      ( JobExecutionStatus',
        JESCanceled,
        JESFailed,
        JESInProgress,
        JESQueued,
        JESRejected,
        JESRemoved,
        JESSucceeded,
        JESTimedOut
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

pattern JESCanceled :: JobExecutionStatus
pattern JESCanceled = JobExecutionStatus' "CANCELED"

pattern JESFailed :: JobExecutionStatus
pattern JESFailed = JobExecutionStatus' "FAILED"

pattern JESInProgress :: JobExecutionStatus
pattern JESInProgress = JobExecutionStatus' "IN_PROGRESS"

pattern JESQueued :: JobExecutionStatus
pattern JESQueued = JobExecutionStatus' "QUEUED"

pattern JESRejected :: JobExecutionStatus
pattern JESRejected = JobExecutionStatus' "REJECTED"

pattern JESRemoved :: JobExecutionStatus
pattern JESRemoved = JobExecutionStatus' "REMOVED"

pattern JESSucceeded :: JobExecutionStatus
pattern JESSucceeded = JobExecutionStatus' "SUCCEEDED"

pattern JESTimedOut :: JobExecutionStatus
pattern JESTimedOut = JobExecutionStatus' "TIMED_OUT"

{-# COMPLETE
  JESCanceled,
  JESFailed,
  JESInProgress,
  JESQueued,
  JESRejected,
  JESRemoved,
  JESSucceeded,
  JESTimedOut,
  JobExecutionStatus'
  #-}
