{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Location
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.Location
  ( Location (..)
  -- * Smart constructor
  , mkLocation
  -- * Lenses
  , lLatitude
  , lLongitude
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a latitude and longitude pair, expressed in geographic coordinate system degrees (for example, 47.6204, -122.3491).
--
-- Elevation is currently not supported.
--
-- /See:/ 'mkLocation' smart constructor.
data Location = Location'
  { latitude :: Core.Double
    -- ^ The latitude.
  , longitude :: Core.Double
    -- ^ The longitude.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Location' value with any optional fields omitted.
mkLocation
    :: Core.Double -- ^ 'latitude'
    -> Core.Double -- ^ 'longitude'
    -> Location
mkLocation latitude longitude = Location'{latitude, longitude}

-- | The latitude.
--
-- /Note:/ Consider using 'latitude' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lLatitude :: Lens.Lens' Location Core.Double
lLatitude = Lens.field @"latitude"
{-# INLINEABLE lLatitude #-}
{-# DEPRECATED latitude "Use generic-lens or generic-optics with 'latitude' instead"  #-}

-- | The longitude.
--
-- /Note:/ Consider using 'longitude' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lLongitude :: Lens.Lens' Location Core.Double
lLongitude = Lens.field @"longitude"
{-# INLINEABLE lLongitude #-}
{-# DEPRECATED longitude "Use generic-lens or generic-optics with 'longitude' instead"  #-}

instance Core.FromJSON Location where
        toJSON Location{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("latitude" Core..= latitude),
                  Core.Just ("longitude" Core..= longitude)])

instance Core.FromJSON Location where
        parseJSON
          = Core.withObject "Location" Core.$
              \ x ->
                Location' Core.<$>
                  (x Core..: "latitude") Core.<*> x Core..: "longitude"
