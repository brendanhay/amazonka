{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.RemoteIpDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.RemoteIpDetails
  ( RemoteIpDetails (..)
  -- * Smart constructor
  , mkRemoteIpDetails
  -- * Lenses
  , ridCity
  , ridCountry
  , ridGeoLocation
  , ridIpAddressV4
  , ridOrganization
  ) where

import qualified Network.AWS.GuardDuty.Types.City as Types
import qualified Network.AWS.GuardDuty.Types.Country as Types
import qualified Network.AWS.GuardDuty.Types.GeoLocation as Types
import qualified Network.AWS.GuardDuty.Types.Organization as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the remote IP address of the connection.
--
-- /See:/ 'mkRemoteIpDetails' smart constructor.
data RemoteIpDetails = RemoteIpDetails'
  { city :: Core.Maybe Types.City
    -- ^ The city information of the remote IP address.
  , country :: Core.Maybe Types.Country
    -- ^ The country code of the remote IP address.
  , geoLocation :: Core.Maybe Types.GeoLocation
    -- ^ The location information of the remote IP address.
  , ipAddressV4 :: Core.Maybe Core.Text
    -- ^ The IPv4 remote address of the connection.
  , organization :: Core.Maybe Types.Organization
    -- ^ The ISP organization information of the remote IP address.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RemoteIpDetails' value with any optional fields omitted.
mkRemoteIpDetails
    :: RemoteIpDetails
mkRemoteIpDetails
  = RemoteIpDetails'{city = Core.Nothing, country = Core.Nothing,
                     geoLocation = Core.Nothing, ipAddressV4 = Core.Nothing,
                     organization = Core.Nothing}

-- | The city information of the remote IP address.
--
-- /Note:/ Consider using 'city' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ridCity :: Lens.Lens' RemoteIpDetails (Core.Maybe Types.City)
ridCity = Lens.field @"city"
{-# INLINEABLE ridCity #-}
{-# DEPRECATED city "Use generic-lens or generic-optics with 'city' instead"  #-}

-- | The country code of the remote IP address.
--
-- /Note:/ Consider using 'country' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ridCountry :: Lens.Lens' RemoteIpDetails (Core.Maybe Types.Country)
ridCountry = Lens.field @"country"
{-# INLINEABLE ridCountry #-}
{-# DEPRECATED country "Use generic-lens or generic-optics with 'country' instead"  #-}

-- | The location information of the remote IP address.
--
-- /Note:/ Consider using 'geoLocation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ridGeoLocation :: Lens.Lens' RemoteIpDetails (Core.Maybe Types.GeoLocation)
ridGeoLocation = Lens.field @"geoLocation"
{-# INLINEABLE ridGeoLocation #-}
{-# DEPRECATED geoLocation "Use generic-lens or generic-optics with 'geoLocation' instead"  #-}

-- | The IPv4 remote address of the connection.
--
-- /Note:/ Consider using 'ipAddressV4' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ridIpAddressV4 :: Lens.Lens' RemoteIpDetails (Core.Maybe Core.Text)
ridIpAddressV4 = Lens.field @"ipAddressV4"
{-# INLINEABLE ridIpAddressV4 #-}
{-# DEPRECATED ipAddressV4 "Use generic-lens or generic-optics with 'ipAddressV4' instead"  #-}

-- | The ISP organization information of the remote IP address.
--
-- /Note:/ Consider using 'organization' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ridOrganization :: Lens.Lens' RemoteIpDetails (Core.Maybe Types.Organization)
ridOrganization = Lens.field @"organization"
{-# INLINEABLE ridOrganization #-}
{-# DEPRECATED organization "Use generic-lens or generic-optics with 'organization' instead"  #-}

instance Core.FromJSON RemoteIpDetails where
        parseJSON
          = Core.withObject "RemoteIpDetails" Core.$
              \ x ->
                RemoteIpDetails' Core.<$>
                  (x Core..:? "city") Core.<*> x Core..:? "country" Core.<*>
                    x Core..:? "geoLocation"
                    Core.<*> x Core..:? "ipAddressV4"
                    Core.<*> x Core..:? "organization"
