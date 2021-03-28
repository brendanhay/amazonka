{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.EndpointLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.EndpointLocation
  ( EndpointLocation (..)
  -- * Smart constructor
  , mkEndpointLocation
  -- * Lenses
  , elCity
  , elCountry
  , elLatitude
  , elLongitude
  , elPostalCode
  , elRegion
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies geographic information about an endpoint.
--
-- /See:/ 'mkEndpointLocation' smart constructor.
data EndpointLocation = EndpointLocation'
  { city :: Core.Maybe Core.Text
    -- ^ The name of the city where the endpoint is located.
  , country :: Core.Maybe Core.Text
    -- ^ The two-character code, in ISO 3166-1 alpha-2 format, for the country or region where the endpoint is located. For example, US for the United States.
  , latitude :: Core.Maybe Core.Double
    -- ^ The latitude coordinate of the endpoint location, rounded to one decimal place.
  , longitude :: Core.Maybe Core.Double
    -- ^ The longitude coordinate of the endpoint location, rounded to one decimal place.
  , postalCode :: Core.Maybe Core.Text
    -- ^ The postal or ZIP code for the area where the endpoint is located.
  , region :: Core.Maybe Core.Text
    -- ^ The name of the region where the endpoint is located. For locations in the United States, this value is the name of a state.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EndpointLocation' value with any optional fields omitted.
mkEndpointLocation
    :: EndpointLocation
mkEndpointLocation
  = EndpointLocation'{city = Core.Nothing, country = Core.Nothing,
                      latitude = Core.Nothing, longitude = Core.Nothing,
                      postalCode = Core.Nothing, region = Core.Nothing}

-- | The name of the city where the endpoint is located.
--
-- /Note:/ Consider using 'city' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elCity :: Lens.Lens' EndpointLocation (Core.Maybe Core.Text)
elCity = Lens.field @"city"
{-# INLINEABLE elCity #-}
{-# DEPRECATED city "Use generic-lens or generic-optics with 'city' instead"  #-}

-- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or region where the endpoint is located. For example, US for the United States.
--
-- /Note:/ Consider using 'country' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elCountry :: Lens.Lens' EndpointLocation (Core.Maybe Core.Text)
elCountry = Lens.field @"country"
{-# INLINEABLE elCountry #-}
{-# DEPRECATED country "Use generic-lens or generic-optics with 'country' instead"  #-}

-- | The latitude coordinate of the endpoint location, rounded to one decimal place.
--
-- /Note:/ Consider using 'latitude' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elLatitude :: Lens.Lens' EndpointLocation (Core.Maybe Core.Double)
elLatitude = Lens.field @"latitude"
{-# INLINEABLE elLatitude #-}
{-# DEPRECATED latitude "Use generic-lens or generic-optics with 'latitude' instead"  #-}

-- | The longitude coordinate of the endpoint location, rounded to one decimal place.
--
-- /Note:/ Consider using 'longitude' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elLongitude :: Lens.Lens' EndpointLocation (Core.Maybe Core.Double)
elLongitude = Lens.field @"longitude"
{-# INLINEABLE elLongitude #-}
{-# DEPRECATED longitude "Use generic-lens or generic-optics with 'longitude' instead"  #-}

-- | The postal or ZIP code for the area where the endpoint is located.
--
-- /Note:/ Consider using 'postalCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elPostalCode :: Lens.Lens' EndpointLocation (Core.Maybe Core.Text)
elPostalCode = Lens.field @"postalCode"
{-# INLINEABLE elPostalCode #-}
{-# DEPRECATED postalCode "Use generic-lens or generic-optics with 'postalCode' instead"  #-}

-- | The name of the region where the endpoint is located. For locations in the United States, this value is the name of a state.
--
-- /Note:/ Consider using 'region' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
elRegion :: Lens.Lens' EndpointLocation (Core.Maybe Core.Text)
elRegion = Lens.field @"region"
{-# INLINEABLE elRegion #-}
{-# DEPRECATED region "Use generic-lens or generic-optics with 'region' instead"  #-}

instance Core.FromJSON EndpointLocation where
        toJSON EndpointLocation{..}
          = Core.object
              (Core.catMaybes
                 [("City" Core..=) Core.<$> city,
                  ("Country" Core..=) Core.<$> country,
                  ("Latitude" Core..=) Core.<$> latitude,
                  ("Longitude" Core..=) Core.<$> longitude,
                  ("PostalCode" Core..=) Core.<$> postalCode,
                  ("Region" Core..=) Core.<$> region])

instance Core.FromJSON EndpointLocation where
        parseJSON
          = Core.withObject "EndpointLocation" Core.$
              \ x ->
                EndpointLocation' Core.<$>
                  (x Core..:? "City") Core.<*> x Core..:? "Country" Core.<*>
                    x Core..:? "Latitude"
                    Core.<*> x Core..:? "Longitude"
                    Core.<*> x Core..:? "PostalCode"
                    Core.<*> x Core..:? "Region"
