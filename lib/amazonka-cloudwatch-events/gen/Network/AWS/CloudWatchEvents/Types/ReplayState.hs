{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.ReplayState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.ReplayState
  ( ReplayState
      ( ReplayState',
        ReplayStateStarting,
        ReplayStateRunning,
        ReplayStateCancelling,
        ReplayStateCompleted,
        ReplayStateCancelled,
        ReplayStateFailed,
        fromReplayState
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ReplayState = ReplayState' {fromReplayState :: Core.Text}
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

pattern ReplayStateStarting :: ReplayState
pattern ReplayStateStarting = ReplayState' "STARTING"

pattern ReplayStateRunning :: ReplayState
pattern ReplayStateRunning = ReplayState' "RUNNING"

pattern ReplayStateCancelling :: ReplayState
pattern ReplayStateCancelling = ReplayState' "CANCELLING"

pattern ReplayStateCompleted :: ReplayState
pattern ReplayStateCompleted = ReplayState' "COMPLETED"

pattern ReplayStateCancelled :: ReplayState
pattern ReplayStateCancelled = ReplayState' "CANCELLED"

pattern ReplayStateFailed :: ReplayState
pattern ReplayStateFailed = ReplayState' "FAILED"

{-# COMPLETE
  ReplayStateStarting,
  ReplayStateRunning,
  ReplayStateCancelling,
  ReplayStateCompleted,
  ReplayStateCancelled,
  ReplayStateFailed,
  ReplayState'
  #-}
