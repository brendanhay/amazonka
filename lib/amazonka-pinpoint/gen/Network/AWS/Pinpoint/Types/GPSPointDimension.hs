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
    gpspdRangeInKilometers,
    gpspdCoordinates,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.GPSCoordinates
import qualified Network.AWS.Prelude as Lude

-- | Specifies GPS-based criteria for including or excluding endpoints from a segment.
--
-- /See:/ 'mkGPSPointDimension' smart constructor.
data GPSPointDimension = GPSPointDimension'
  { rangeInKilometers ::
      Lude.Maybe Lude.Double,
    coordinates :: GPSCoordinates
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GPSPointDimension' with the minimum fields required to make a request.
--
-- * 'coordinates' - The GPS coordinates to measure distance from.
-- * 'rangeInKilometers' - The range, in kilometers, from the GPS coordinates.
mkGPSPointDimension ::
  -- | 'coordinates'
  GPSCoordinates ->
  GPSPointDimension
mkGPSPointDimension pCoordinates_ =
  GPSPointDimension'
    { rangeInKilometers = Lude.Nothing,
      coordinates = pCoordinates_
    }

-- | The range, in kilometers, from the GPS coordinates.
--
-- /Note:/ Consider using 'rangeInKilometers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpspdRangeInKilometers :: Lens.Lens' GPSPointDimension (Lude.Maybe Lude.Double)
gpspdRangeInKilometers = Lens.lens (rangeInKilometers :: GPSPointDimension -> Lude.Maybe Lude.Double) (\s a -> s {rangeInKilometers = a} :: GPSPointDimension)
{-# DEPRECATED gpspdRangeInKilometers "Use generic-lens or generic-optics with 'rangeInKilometers' instead." #-}

-- | The GPS coordinates to measure distance from.
--
-- /Note:/ Consider using 'coordinates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpspdCoordinates :: Lens.Lens' GPSPointDimension GPSCoordinates
gpspdCoordinates = Lens.lens (coordinates :: GPSPointDimension -> GPSCoordinates) (\s a -> s {coordinates = a} :: GPSPointDimension)
{-# DEPRECATED gpspdCoordinates "Use generic-lens or generic-optics with 'coordinates' instead." #-}

instance Lude.FromJSON GPSPointDimension where
  parseJSON =
    Lude.withObject
      "GPSPointDimension"
      ( \x ->
          GPSPointDimension'
            Lude.<$> (x Lude..:? "RangeInKilometers")
            Lude.<*> (x Lude..: "Coordinates")
      )

instance Lude.ToJSON GPSPointDimension where
  toJSON GPSPointDimension' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RangeInKilometers" Lude..=) Lude.<$> rangeInKilometers,
            Lude.Just ("Coordinates" Lude..= coordinates)
          ]
      )
