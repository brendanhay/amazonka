-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.JobRunState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.JobRunState
  ( JobRunState
      ( JobRunState',
        JRSFailed,
        JRSRunning,
        JRSStarting,
        JRSStopped,
        JRSStopping,
        JRSSucceeded,
        JRSTimeout
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype JobRunState = JobRunState' Lude.Text
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

pattern JRSFailed :: JobRunState
pattern JRSFailed = JobRunState' "FAILED"

pattern JRSRunning :: JobRunState
pattern JRSRunning = JobRunState' "RUNNING"

pattern JRSStarting :: JobRunState
pattern JRSStarting = JobRunState' "STARTING"

pattern JRSStopped :: JobRunState
pattern JRSStopped = JobRunState' "STOPPED"

pattern JRSStopping :: JobRunState
pattern JRSStopping = JobRunState' "STOPPING"

pattern JRSSucceeded :: JobRunState
pattern JRSSucceeded = JobRunState' "SUCCEEDED"

pattern JRSTimeout :: JobRunState
pattern JRSTimeout = JobRunState' "TIMEOUT"

{-# COMPLETE
  JRSFailed,
  JRSRunning,
  JRSStarting,
  JRSStopped,
  JRSStopping,
  JRSSucceeded,
  JRSTimeout,
  JobRunState'
  #-}
