{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.ChannelState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.ChannelState
  ( ChannelState
      ( ChannelState',
        ChannelStateCreating,
        ChannelStateCreateFailed,
        ChannelStateIdle,
        ChannelStateStarting,
        ChannelStateRunning,
        ChannelStateRecovering,
        ChannelStateStopping,
        ChannelStateDeleting,
        ChannelStateDeleted,
        ChannelStateUpdating,
        ChannelStateUpdateFailed,
        fromChannelState
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | Placeholder documentation for ChannelState
newtype ChannelState = ChannelState' {fromChannelState :: Core.Text}
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

pattern ChannelStateCreating :: ChannelState
pattern ChannelStateCreating = ChannelState' "CREATING"

pattern ChannelStateCreateFailed :: ChannelState
pattern ChannelStateCreateFailed = ChannelState' "CREATE_FAILED"

pattern ChannelStateIdle :: ChannelState
pattern ChannelStateIdle = ChannelState' "IDLE"

pattern ChannelStateStarting :: ChannelState
pattern ChannelStateStarting = ChannelState' "STARTING"

pattern ChannelStateRunning :: ChannelState
pattern ChannelStateRunning = ChannelState' "RUNNING"

pattern ChannelStateRecovering :: ChannelState
pattern ChannelStateRecovering = ChannelState' "RECOVERING"

pattern ChannelStateStopping :: ChannelState
pattern ChannelStateStopping = ChannelState' "STOPPING"

pattern ChannelStateDeleting :: ChannelState
pattern ChannelStateDeleting = ChannelState' "DELETING"

pattern ChannelStateDeleted :: ChannelState
pattern ChannelStateDeleted = ChannelState' "DELETED"

pattern ChannelStateUpdating :: ChannelState
pattern ChannelStateUpdating = ChannelState' "UPDATING"

pattern ChannelStateUpdateFailed :: ChannelState
pattern ChannelStateUpdateFailed = ChannelState' "UPDATE_FAILED"

{-# COMPLETE
  ChannelStateCreating,
  ChannelStateCreateFailed,
  ChannelStateIdle,
  ChannelStateStarting,
  ChannelStateRunning,
  ChannelStateRecovering,
  ChannelStateStopping,
  ChannelStateDeleting,
  ChannelStateDeleted,
  ChannelStateUpdating,
  ChannelStateUpdateFailed,
  ChannelState'
  #-}
