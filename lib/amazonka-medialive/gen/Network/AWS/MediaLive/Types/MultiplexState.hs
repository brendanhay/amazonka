{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaLive.Types.MultiplexState
  ( MultiplexState
    ( MultiplexState'
    , MultiplexStateCreating
    , MultiplexStateCreateFailed
    , MultiplexStateIdle
    , MultiplexStateStarting
    , MultiplexStateRunning
    , MultiplexStateRecovering
    , MultiplexStateStopping
    , MultiplexStateDeleting
    , MultiplexStateDeleted
    , fromMultiplexState
    )
  ) where

import qualified Network.AWS.Prelude as Core

-- | The current state of the multiplex.
newtype MultiplexState = MultiplexState'{fromMultiplexState ::
                                         Core.Text}
                           deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                           Core.Generic)
                           deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                             Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                             Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                             Core.FromText, Core.ToByteString, Core.ToQuery,
                                             Core.ToHeader)

pattern MultiplexStateCreating :: MultiplexState
pattern MultiplexStateCreating = MultiplexState' "CREATING"

pattern MultiplexStateCreateFailed :: MultiplexState
pattern MultiplexStateCreateFailed = MultiplexState' "CREATE_FAILED"

pattern MultiplexStateIdle :: MultiplexState
pattern MultiplexStateIdle = MultiplexState' "IDLE"

pattern MultiplexStateStarting :: MultiplexState
pattern MultiplexStateStarting = MultiplexState' "STARTING"

pattern MultiplexStateRunning :: MultiplexState
pattern MultiplexStateRunning = MultiplexState' "RUNNING"

pattern MultiplexStateRecovering :: MultiplexState
pattern MultiplexStateRecovering = MultiplexState' "RECOVERING"

pattern MultiplexStateStopping :: MultiplexState
pattern MultiplexStateStopping = MultiplexState' "STOPPING"

pattern MultiplexStateDeleting :: MultiplexState
pattern MultiplexStateDeleting = MultiplexState' "DELETING"

pattern MultiplexStateDeleted :: MultiplexState
pattern MultiplexStateDeleted = MultiplexState' "DELETED"

{-# COMPLETE 
  MultiplexStateCreating,

  MultiplexStateCreateFailed,

  MultiplexStateIdle,

  MultiplexStateStarting,

  MultiplexStateRunning,

  MultiplexStateRecovering,

  MultiplexStateStopping,

  MultiplexStateDeleting,

  MultiplexStateDeleted,
  MultiplexState'
  #-}
