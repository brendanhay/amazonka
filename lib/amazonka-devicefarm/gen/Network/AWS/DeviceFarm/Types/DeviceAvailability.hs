{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.DeviceAvailability
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.DeviceAvailability
  ( DeviceAvailability
    ( DeviceAvailability'
    , DeviceAvailabilityTemporaryNotAvailable
    , DeviceAvailabilityBusy
    , DeviceAvailabilityAvailable
    , DeviceAvailabilityHighlyAvailable
    , fromDeviceAvailability
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype DeviceAvailability = DeviceAvailability'{fromDeviceAvailability
                                                 :: Core.Text}
                               deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                               Core.Generic)
                               deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                 Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                 Core.FromJSON, Core.ToXML, Core.FromXML,
                                                 Core.ToText, Core.FromText, Core.ToByteString,
                                                 Core.ToQuery, Core.ToHeader)

pattern DeviceAvailabilityTemporaryNotAvailable :: DeviceAvailability
pattern DeviceAvailabilityTemporaryNotAvailable = DeviceAvailability' "TEMPORARY_NOT_AVAILABLE"

pattern DeviceAvailabilityBusy :: DeviceAvailability
pattern DeviceAvailabilityBusy = DeviceAvailability' "BUSY"

pattern DeviceAvailabilityAvailable :: DeviceAvailability
pattern DeviceAvailabilityAvailable = DeviceAvailability' "AVAILABLE"

pattern DeviceAvailabilityHighlyAvailable :: DeviceAvailability
pattern DeviceAvailabilityHighlyAvailable = DeviceAvailability' "HIGHLY_AVAILABLE"

{-# COMPLETE 
  DeviceAvailabilityTemporaryNotAvailable,

  DeviceAvailabilityBusy,

  DeviceAvailabilityAvailable,

  DeviceAvailabilityHighlyAvailable,
  DeviceAvailability'
  #-}
