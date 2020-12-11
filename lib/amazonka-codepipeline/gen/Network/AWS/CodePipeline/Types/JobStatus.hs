-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.JobStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.JobStatus
  ( JobStatus
      ( JobStatus',
        JSCreated,
        JSDispatched,
        JSFailed,
        JSInProgress,
        JSQueued,
        JSSucceeded,
        JSTimedOut
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
pattern JSCreated = JobStatus' "Created"

pattern JSDispatched :: JobStatus
pattern JSDispatched = JobStatus' "Dispatched"

pattern JSFailed :: JobStatus
pattern JSFailed = JobStatus' "Failed"

pattern JSInProgress :: JobStatus
pattern JSInProgress = JobStatus' "InProgress"

pattern JSQueued :: JobStatus
pattern JSQueued = JobStatus' "Queued"

pattern JSSucceeded :: JobStatus
pattern JSSucceeded = JobStatus' "Succeeded"

pattern JSTimedOut :: JobStatus
pattern JSTimedOut = JobStatus' "TimedOut"

{-# COMPLETE
  JSCreated,
  JSDispatched,
  JSFailed,
  JSInProgress,
  JSQueued,
  JSSucceeded,
  JSTimedOut,
  JobStatus'
  #-}
