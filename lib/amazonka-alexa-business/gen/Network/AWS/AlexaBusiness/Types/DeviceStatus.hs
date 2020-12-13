{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
        Ready,
        Pending,
        WasOffline,
        Deregistered,
        Failed
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

pattern Ready :: DeviceStatus
pattern Ready = DeviceStatus' "READY"

pattern Pending :: DeviceStatus
pattern Pending = DeviceStatus' "PENDING"

pattern WasOffline :: DeviceStatus
pattern WasOffline = DeviceStatus' "WAS_OFFLINE"

pattern Deregistered :: DeviceStatus
pattern Deregistered = DeviceStatus' "DEREGISTERED"

pattern Failed :: DeviceStatus
pattern Failed = DeviceStatus' "FAILED"

{-# COMPLETE
  Ready,
  Pending,
  WasOffline,
  Deregistered,
  Failed,
  DeviceStatus'
  #-}
