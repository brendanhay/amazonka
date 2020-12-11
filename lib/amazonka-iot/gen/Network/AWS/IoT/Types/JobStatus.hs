-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.JobStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.JobStatus
  ( JobStatus
      ( JobStatus',
        JSCanceled,
        JSCompleted,
        JSDeletionInProgress,
        JSInProgress
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

pattern JSCanceled :: JobStatus
pattern JSCanceled = JobStatus' "CANCELED"

pattern JSCompleted :: JobStatus
pattern JSCompleted = JobStatus' "COMPLETED"

pattern JSDeletionInProgress :: JobStatus
pattern JSDeletionInProgress = JobStatus' "DELETION_IN_PROGRESS"

pattern JSInProgress :: JobStatus
pattern JSInProgress = JobStatus' "IN_PROGRESS"

{-# COMPLETE
  JSCanceled,
  JSCompleted,
  JSDeletionInProgress,
  JSInProgress,
  JobStatus'
  #-}
