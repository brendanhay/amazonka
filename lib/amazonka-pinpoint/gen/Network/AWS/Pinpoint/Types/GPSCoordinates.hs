{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.GPSCoordinates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.GPSCoordinates
  ( GPSCoordinates (..),

    -- * Smart constructor
    mkGPSCoordinates,

    -- * Lenses
    gpscLatitude,
    gpscLongitude,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the GPS coordinates of a location.
--
-- /See:/ 'mkGPSCoordinates' smart constructor.
data GPSCoordinates = GPSCoordinates'
  { -- | The latitude coordinate of the location.
    latitude :: Core.Double,
    -- | The longitude coordinate of the location.
    longitude :: Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GPSCoordinates' value with any optional fields omitted.
mkGPSCoordinates ::
  -- | 'latitude'
  Core.Double ->
  -- | 'longitude'
  Core.Double ->
  GPSCoordinates
mkGPSCoordinates latitude longitude =
  GPSCoordinates' {latitude, longitude}

-- | The latitude coordinate of the location.
--
-- /Note:/ Consider using 'latitude' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpscLatitude :: Lens.Lens' GPSCoordinates Core.Double
gpscLatitude = Lens.field @"latitude"
{-# DEPRECATED gpscLatitude "Use generic-lens or generic-optics with 'latitude' instead." #-}

-- | The longitude coordinate of the location.
--
-- /Note:/ Consider using 'longitude' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpscLongitude :: Lens.Lens' GPSCoordinates Core.Double
gpscLongitude = Lens.field @"longitude"
{-# DEPRECATED gpscLongitude "Use generic-lens or generic-optics with 'longitude' instead." #-}

instance Core.FromJSON GPSCoordinates where
  toJSON GPSCoordinates {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Latitude" Core..= latitude),
            Core.Just ("Longitude" Core..= longitude)
          ]
      )

instance Core.FromJSON GPSCoordinates where
  parseJSON =
    Core.withObject "GPSCoordinates" Core.$
      \x ->
        GPSCoordinates'
          Core.<$> (x Core..: "Latitude") Core.<*> (x Core..: "Longitude")
