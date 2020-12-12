{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.JobStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.JobStatus
  ( JobStatus
      ( JobStatus',
        Failed,
        Pending,
        Runnable,
        Running,
        Starting,
        Submitted,
        Succeeded
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

pattern Failed :: JobStatus
pattern Failed = JobStatus' "FAILED"

pattern Pending :: JobStatus
pattern Pending = JobStatus' "PENDING"

pattern Runnable :: JobStatus
pattern Runnable = JobStatus' "RUNNABLE"

pattern Running :: JobStatus
pattern Running = JobStatus' "RUNNING"

pattern Starting :: JobStatus
pattern Starting = JobStatus' "STARTING"

pattern Submitted :: JobStatus
pattern Submitted = JobStatus' "SUBMITTED"

pattern Succeeded :: JobStatus
pattern Succeeded = JobStatus' "SUCCEEDED"

{-# COMPLETE
  Failed,
  Pending,
  Runnable,
  Running,
  Starting,
  Submitted,
  Succeeded,
  JobStatus'
  #-}
