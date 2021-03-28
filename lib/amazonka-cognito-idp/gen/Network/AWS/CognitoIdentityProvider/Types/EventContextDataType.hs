{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.EventContextDataType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.EventContextDataType
  ( EventContextDataType (..)
  -- * Smart constructor
  , mkEventContextDataType
  -- * Lenses
  , ecdtCity
  , ecdtCountry
  , ecdtDeviceName
  , ecdtIpAddress
  , ecdtTimezone
  ) where

import qualified Network.AWS.CognitoIdentityProvider.Types.StringType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the user context data captured at the time of an event request.
--
-- /See:/ 'mkEventContextDataType' smart constructor.
data EventContextDataType = EventContextDataType'
  { city :: Core.Maybe Types.StringType
    -- ^ The user's city.
  , country :: Core.Maybe Types.StringType
    -- ^ The user's country.
  , deviceName :: Core.Maybe Types.StringType
    -- ^ The user's device name.
  , ipAddress :: Core.Maybe Types.StringType
    -- ^ The user's IP address.
  , timezone :: Core.Maybe Types.StringType
    -- ^ The user's time zone.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EventContextDataType' value with any optional fields omitted.
mkEventContextDataType
    :: EventContextDataType
mkEventContextDataType
  = EventContextDataType'{city = Core.Nothing,
                          country = Core.Nothing, deviceName = Core.Nothing,
                          ipAddress = Core.Nothing, timezone = Core.Nothing}

-- | The user's city.
--
-- /Note:/ Consider using 'city' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecdtCity :: Lens.Lens' EventContextDataType (Core.Maybe Types.StringType)
ecdtCity = Lens.field @"city"
{-# INLINEABLE ecdtCity #-}
{-# DEPRECATED city "Use generic-lens or generic-optics with 'city' instead"  #-}

-- | The user's country.
--
-- /Note:/ Consider using 'country' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecdtCountry :: Lens.Lens' EventContextDataType (Core.Maybe Types.StringType)
ecdtCountry = Lens.field @"country"
{-# INLINEABLE ecdtCountry #-}
{-# DEPRECATED country "Use generic-lens or generic-optics with 'country' instead"  #-}

-- | The user's device name.
--
-- /Note:/ Consider using 'deviceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecdtDeviceName :: Lens.Lens' EventContextDataType (Core.Maybe Types.StringType)
ecdtDeviceName = Lens.field @"deviceName"
{-# INLINEABLE ecdtDeviceName #-}
{-# DEPRECATED deviceName "Use generic-lens or generic-optics with 'deviceName' instead"  #-}

-- | The user's IP address.
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecdtIpAddress :: Lens.Lens' EventContextDataType (Core.Maybe Types.StringType)
ecdtIpAddress = Lens.field @"ipAddress"
{-# INLINEABLE ecdtIpAddress #-}
{-# DEPRECATED ipAddress "Use generic-lens or generic-optics with 'ipAddress' instead"  #-}

-- | The user's time zone.
--
-- /Note:/ Consider using 'timezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecdtTimezone :: Lens.Lens' EventContextDataType (Core.Maybe Types.StringType)
ecdtTimezone = Lens.field @"timezone"
{-# INLINEABLE ecdtTimezone #-}
{-# DEPRECATED timezone "Use generic-lens or generic-optics with 'timezone' instead"  #-}

instance Core.FromJSON EventContextDataType where
        parseJSON
          = Core.withObject "EventContextDataType" Core.$
              \ x ->
                EventContextDataType' Core.<$>
                  (x Core..:? "City") Core.<*> x Core..:? "Country" Core.<*>
                    x Core..:? "DeviceName"
                    Core.<*> x Core..:? "IpAddress"
                    Core.<*> x Core..:? "Timezone"
