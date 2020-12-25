{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.GPSPointDimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.GPSPointDimension
  ( GPSPointDimension (..),

    -- * Smart constructor
    mkGPSPointDimension,

    -- * Lenses
    gpspdCoordinates,
    gpspdRangeInKilometers,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.GPSCoordinates as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies GPS-based criteria for including or excluding endpoints from a segment.
--
-- /See:/ 'mkGPSPointDimension' smart constructor.
data GPSPointDimension = GPSPointDimension'
  { -- | The GPS coordinates to measure distance from.
    coordinates :: Types.GPSCoordinates,
    -- | The range, in kilometers, from the GPS coordinates.
    rangeInKilometers :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GPSPointDimension' value with any optional fields omitted.
mkGPSPointDimension ::
  -- | 'coordinates'
  Types.GPSCoordinates ->
  GPSPointDimension
mkGPSPointDimension coordinates =
  GPSPointDimension' {coordinates, rangeInKilometers = Core.Nothing}

-- | The GPS coordinates to measure distance from.
--
-- /Note:/ Consider using 'coordinates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpspdCoordinates :: Lens.Lens' GPSPointDimension Types.GPSCoordinates
gpspdCoordinates = Lens.field @"coordinates"
{-# DEPRECATED gpspdCoordinates "Use generic-lens or generic-optics with 'coordinates' instead." #-}

-- | The range, in kilometers, from the GPS coordinates.
--
-- /Note:/ Consider using 'rangeInKilometers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpspdRangeInKilometers :: Lens.Lens' GPSPointDimension (Core.Maybe Core.Double)
gpspdRangeInKilometers = Lens.field @"rangeInKilometers"
{-# DEPRECATED gpspdRangeInKilometers "Use generic-lens or generic-optics with 'rangeInKilometers' instead." #-}

instance Core.FromJSON GPSPointDimension where
  toJSON GPSPointDimension {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Coordinates" Core..= coordinates),
            ("RangeInKilometers" Core..=) Core.<$> rangeInKilometers
          ]
      )

instance Core.FromJSON GPSPointDimension where
  parseJSON =
    Core.withObject "GPSPointDimension" Core.$
      \x ->
        GPSPointDimension'
          Core.<$> (x Core..: "Coordinates") Core.<*> (x Core..:? "RangeInKilometers")
