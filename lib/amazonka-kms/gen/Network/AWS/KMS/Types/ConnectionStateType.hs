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
        Connected,
        Connecting,
        Disconnected,
        Disconnecting,
        Failed
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype ConnectionStateType = ConnectionStateType' Lude.Text
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

pattern Connected :: ConnectionStateType
pattern Connected = ConnectionStateType' "CONNECTED"

pattern Connecting :: ConnectionStateType
pattern Connecting = ConnectionStateType' "CONNECTING"

pattern Disconnected :: ConnectionStateType
pattern Disconnected = ConnectionStateType' "DISCONNECTED"

pattern Disconnecting :: ConnectionStateType
pattern Disconnecting = ConnectionStateType' "DISCONNECTING"

pattern Failed :: ConnectionStateType
pattern Failed = ConnectionStateType' "FAILED"

{-# COMPLETE
  Connected,
  Connecting,
  Disconnected,
  Disconnecting,
  Failed,
  ConnectionStateType'
  #-}
