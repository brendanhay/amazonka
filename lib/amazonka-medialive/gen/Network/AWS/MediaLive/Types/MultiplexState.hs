{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.MultiplexState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.MultiplexState
  ( MultiplexState
      ( MultiplexState',
        MSCreateFailed,
        MSCreating,
        MSDeleted,
        MSDeleting,
        MSIdle,
        MSRecovering,
        MSRunning,
        MSStarting,
        MSStopping
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | The current state of the multiplex.
newtype MultiplexState = MultiplexState' Lude.Text
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

pattern MSCreateFailed :: MultiplexState
pattern MSCreateFailed = MultiplexState' "CREATE_FAILED"

pattern MSCreating :: MultiplexState
pattern MSCreating = MultiplexState' "CREATING"

pattern MSDeleted :: MultiplexState
pattern MSDeleted = MultiplexState' "DELETED"

pattern MSDeleting :: MultiplexState
pattern MSDeleting = MultiplexState' "DELETING"

pattern MSIdle :: MultiplexState
pattern MSIdle = MultiplexState' "IDLE"

pattern MSRecovering :: MultiplexState
pattern MSRecovering = MultiplexState' "RECOVERING"

pattern MSRunning :: MultiplexState
pattern MSRunning = MultiplexState' "RUNNING"

pattern MSStarting :: MultiplexState
pattern MSStarting = MultiplexState' "STARTING"

pattern MSStopping :: MultiplexState
pattern MSStopping = MultiplexState' "STOPPING"

{-# COMPLETE
  MSCreateFailed,
  MSCreating,
  MSDeleted,
  MSDeleting,
  MSIdle,
  MSRecovering,
  MSRunning,
  MSStarting,
  MSStopping,
  MultiplexState'
  #-}
