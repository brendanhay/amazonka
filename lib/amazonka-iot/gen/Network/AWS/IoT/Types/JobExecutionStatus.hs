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
        JESQueued,
        JESInProgress,
        JESSucceeded,
        JESFailed,
        JESTimedOut,
        JESRejected,
        JESRemoved,
        JESCanceled
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

pattern JESQueued :: JobExecutionStatus
pattern JESQueued = JobExecutionStatus' "QUEUED"

pattern JESInProgress :: JobExecutionStatus
pattern JESInProgress = JobExecutionStatus' "IN_PROGRESS"

pattern JESSucceeded :: JobExecutionStatus
pattern JESSucceeded = JobExecutionStatus' "SUCCEEDED"

pattern JESFailed :: JobExecutionStatus
pattern JESFailed = JobExecutionStatus' "FAILED"

pattern JESTimedOut :: JobExecutionStatus
pattern JESTimedOut = JobExecutionStatus' "TIMED_OUT"

pattern JESRejected :: JobExecutionStatus
pattern JESRejected = JobExecutionStatus' "REJECTED"

pattern JESRemoved :: JobExecutionStatus
pattern JESRemoved = JobExecutionStatus' "REMOVED"

pattern JESCanceled :: JobExecutionStatus
pattern JESCanceled = JobExecutionStatus' "CANCELED"

{-# COMPLETE
  JESQueued,
  JESInProgress,
  JESSucceeded,
  JESFailed,
  JESTimedOut,
  JESRejected,
  JESRemoved,
  JESCanceled,
  JobExecutionStatus'
  #-}
