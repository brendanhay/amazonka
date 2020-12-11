-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.DeviceStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.DeviceStatus
  ( DeviceStatus
      ( DeviceStatus',
        Deregistered,
        Failed,
        Pending,
        Ready,
        WasOffline
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DeviceStatus = DeviceStatus' Lude.Text
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

pattern Deregistered :: DeviceStatus
pattern Deregistered = DeviceStatus' "DEREGISTERED"

pattern Failed :: DeviceStatus
pattern Failed = DeviceStatus' "FAILED"

pattern Pending :: DeviceStatus
pattern Pending = DeviceStatus' "PENDING"

pattern Ready :: DeviceStatus
pattern Ready = DeviceStatus' "READY"

pattern WasOffline :: DeviceStatus
pattern WasOffline = DeviceStatus' "WAS_OFFLINE"

{-# COMPLETE
  Deregistered,
  Failed,
  Pending,
  Ready,
  WasOffline,
  DeviceStatus'
  #-}
