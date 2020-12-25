{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputDeviceConnectionState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputDeviceConnectionState
  ( InputDeviceConnectionState
      ( InputDeviceConnectionState',
        InputDeviceConnectionStateDisconnected,
        InputDeviceConnectionStateConnected,
        fromInputDeviceConnectionState
      ),
  )
where

import qualified Network.AWS.Prelude as Core

-- | The state of the connection between the input device and AWS.
newtype InputDeviceConnectionState = InputDeviceConnectionState'
  { fromInputDeviceConnectionState ::
      Core.Text
  }
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

pattern InputDeviceConnectionStateDisconnected :: InputDeviceConnectionState
pattern InputDeviceConnectionStateDisconnected = InputDeviceConnectionState' "DISCONNECTED"

pattern InputDeviceConnectionStateConnected :: InputDeviceConnectionState
pattern InputDeviceConnectionStateConnected = InputDeviceConnectionState' "CONNECTED"

{-# COMPLETE
  InputDeviceConnectionStateDisconnected,
  InputDeviceConnectionStateConnected,
  InputDeviceConnectionState'
  #-}
