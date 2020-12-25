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
        JobRunStateStarting,
        JobRunStateRunning,
        JobRunStateStopping,
        JobRunStateStopped,
        JobRunStateSucceeded,
        JobRunStateFailed,
        JobRunStateTimeout,
        fromJobRunState
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype JobRunState = JobRunState' {fromJobRunState :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern JobRunStateStarting :: JobRunState
pattern JobRunStateStarting = JobRunState' "STARTING"

pattern JobRunStateRunning :: JobRunState
pattern JobRunStateRunning = JobRunState' "RUNNING"

pattern JobRunStateStopping :: JobRunState
pattern JobRunStateStopping = JobRunState' "STOPPING"

pattern JobRunStateStopped :: JobRunState
pattern JobRunStateStopped = JobRunState' "STOPPED"

pattern JobRunStateSucceeded :: JobRunState
pattern JobRunStateSucceeded = JobRunState' "SUCCEEDED"

pattern JobRunStateFailed :: JobRunState
pattern JobRunStateFailed = JobRunState' "FAILED"

pattern JobRunStateTimeout :: JobRunState
pattern JobRunStateTimeout = JobRunState' "TIMEOUT"

{-# COMPLETE
  JobRunStateStarting,
  JobRunStateRunning,
  JobRunStateStopping,
  JobRunStateStopped,
  JobRunStateSucceeded,
  JobRunStateFailed,
  JobRunStateTimeout,
  JobRunState'
  #-}
