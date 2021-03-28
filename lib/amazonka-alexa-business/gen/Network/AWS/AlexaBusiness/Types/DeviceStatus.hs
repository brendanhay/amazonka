{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.DeviceStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.DeviceStatus
  ( DeviceStatus
    ( DeviceStatus'
    , DeviceStatusReady
    , DeviceStatusPending
    , DeviceStatusWasOffline
    , DeviceStatusDeregistered
    , DeviceStatusFailed
    , fromDeviceStatus
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype DeviceStatus = DeviceStatus'{fromDeviceStatus :: Core.Text}
                         deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                         Core.Generic)
                         deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                           Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                           Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                           Core.FromText, Core.ToByteString, Core.ToQuery,
                                           Core.ToHeader)

pattern DeviceStatusReady :: DeviceStatus
pattern DeviceStatusReady = DeviceStatus' "READY"

pattern DeviceStatusPending :: DeviceStatus
pattern DeviceStatusPending = DeviceStatus' "PENDING"

pattern DeviceStatusWasOffline :: DeviceStatus
pattern DeviceStatusWasOffline = DeviceStatus' "WAS_OFFLINE"

pattern DeviceStatusDeregistered :: DeviceStatus
pattern DeviceStatusDeregistered = DeviceStatus' "DEREGISTERED"

pattern DeviceStatusFailed :: DeviceStatus
pattern DeviceStatusFailed = DeviceStatus' "FAILED"

{-# COMPLETE 
  DeviceStatusReady,

  DeviceStatusPending,

  DeviceStatusWasOffline,

  DeviceStatusDeregistered,

  DeviceStatusFailed,
  DeviceStatus'
  #-}
