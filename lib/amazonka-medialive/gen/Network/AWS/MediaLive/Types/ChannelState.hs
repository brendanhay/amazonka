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
        CSCreating,
        CSCreateFailed,
        CSIdle,
        CSStarting,
        CSRunning,
        CSRecovering,
        CSStopping,
        CSDeleting,
        CSDeleted,
        CSUpdating,
        CSUpdateFailed
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

-- | Placeholder documentation for ChannelState
newtype ChannelState = ChannelState' Lude.Text
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

pattern CSCreating :: ChannelState
pattern CSCreating = ChannelState' "CREATING"

pattern CSCreateFailed :: ChannelState
pattern CSCreateFailed = ChannelState' "CREATE_FAILED"

pattern CSIdle :: ChannelState
pattern CSIdle = ChannelState' "IDLE"

pattern CSStarting :: ChannelState
pattern CSStarting = ChannelState' "STARTING"

pattern CSRunning :: ChannelState
pattern CSRunning = ChannelState' "RUNNING"

pattern CSRecovering :: ChannelState
pattern CSRecovering = ChannelState' "RECOVERING"

pattern CSStopping :: ChannelState
pattern CSStopping = ChannelState' "STOPPING"

pattern CSDeleting :: ChannelState
pattern CSDeleting = ChannelState' "DELETING"

pattern CSDeleted :: ChannelState
pattern CSDeleted = ChannelState' "DELETED"

pattern CSUpdating :: ChannelState
pattern CSUpdating = ChannelState' "UPDATING"

pattern CSUpdateFailed :: ChannelState
pattern CSUpdateFailed = ChannelState' "UPDATE_FAILED"

{-# COMPLETE
  CSCreating,
  CSCreateFailed,
  CSIdle,
  CSStarting,
  CSRunning,
  CSRecovering,
  CSStopping,
  CSDeleting,
  CSDeleted,
  CSUpdating,
  CSUpdateFailed,
  ChannelState'
  #-}
