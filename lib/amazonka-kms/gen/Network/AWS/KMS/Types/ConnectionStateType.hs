{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KMS.Types.ConnectionStateType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KMS.Types.ConnectionStateType
  ( ConnectionStateType
      ( ConnectionStateType',
        ConnectionStateTypeConnected,
        ConnectionStateTypeConnecting,
        ConnectionStateTypeFailed,
        ConnectionStateTypeDisconnected,
        ConnectionStateTypeDisconnecting,
        fromConnectionStateType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ConnectionStateType = ConnectionStateType'
  { fromConnectionStateType ::
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

pattern ConnectionStateTypeConnected :: ConnectionStateType
pattern ConnectionStateTypeConnected = ConnectionStateType' "CONNECTED"

pattern ConnectionStateTypeConnecting :: ConnectionStateType
pattern ConnectionStateTypeConnecting = ConnectionStateType' "CONNECTING"

pattern ConnectionStateTypeFailed :: ConnectionStateType
pattern ConnectionStateTypeFailed = ConnectionStateType' "FAILED"

pattern ConnectionStateTypeDisconnected :: ConnectionStateType
pattern ConnectionStateTypeDisconnected = ConnectionStateType' "DISCONNECTED"

pattern ConnectionStateTypeDisconnecting :: ConnectionStateType
pattern ConnectionStateTypeDisconnecting = ConnectionStateType' "DISCONNECTING"

{-# COMPLETE
  ConnectionStateTypeConnected,
  ConnectionStateTypeConnecting,
  ConnectionStateTypeFailed,
  ConnectionStateTypeDisconnected,
  ConnectionStateTypeDisconnecting,
  ConnectionStateType'
  #-}
