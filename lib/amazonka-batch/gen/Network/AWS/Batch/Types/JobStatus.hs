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
        Submitted,
        Pending,
        Runnable,
        Starting,
        Running,
        Succeeded,
        Failed
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

pattern Submitted :: JobStatus
pattern Submitted = JobStatus' "SUBMITTED"

pattern Pending :: JobStatus
pattern Pending = JobStatus' "PENDING"

pattern Runnable :: JobStatus
pattern Runnable = JobStatus' "RUNNABLE"

pattern Starting :: JobStatus
pattern Starting = JobStatus' "STARTING"

pattern Running :: JobStatus
pattern Running = JobStatus' "RUNNING"

pattern Succeeded :: JobStatus
pattern Succeeded = JobStatus' "SUCCEEDED"

pattern Failed :: JobStatus
pattern Failed = JobStatus' "FAILED"

{-# COMPLETE
  Submitted,
  Pending,
  Runnable,
  Starting,
  Running,
  Succeeded,
  Failed,
  JobStatus'
  #-}
