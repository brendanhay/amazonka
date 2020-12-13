{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        JRSStarting,
        JRSRunning,
        JRSStopping,
        JRSStopped,
        JRSSucceeded,
        JRSFailed,
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

pattern JRSStarting :: JobRunState
pattern JRSStarting = JobRunState' "STARTING"

pattern JRSRunning :: JobRunState
pattern JRSRunning = JobRunState' "RUNNING"

pattern JRSStopping :: JobRunState
pattern JRSStopping = JobRunState' "STOPPING"

pattern JRSStopped :: JobRunState
pattern JRSStopped = JobRunState' "STOPPED"

pattern JRSSucceeded :: JobRunState
pattern JRSSucceeded = JobRunState' "SUCCEEDED"

pattern JRSFailed :: JobRunState
pattern JRSFailed = JobRunState' "FAILED"

pattern JRSTimeout :: JobRunState
pattern JRSTimeout = JobRunState' "TIMEOUT"

{-# COMPLETE
  JRSStarting,
  JRSRunning,
  JRSStopping,
  JRSStopped,
  JRSSucceeded,
  JRSFailed,
  JRSTimeout,
  JobRunState'
  #-}
