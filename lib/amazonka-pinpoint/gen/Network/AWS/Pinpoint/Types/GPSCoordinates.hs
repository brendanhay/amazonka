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
import qualified Network.AWS.Prelude as Lude

-- | Specifies the GPS coordinates of a location.
--
-- /See:/ 'mkGPSCoordinates' smart constructor.
data GPSCoordinates = GPSCoordinates'
  { -- | The latitude coordinate of the location.
    latitude :: Lude.Double,
    -- | The longitude coordinate of the location.
    longitude :: Lude.Double
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GPSCoordinates' with the minimum fields required to make a request.
--
-- * 'latitude' - The latitude coordinate of the location.
-- * 'longitude' - The longitude coordinate of the location.
mkGPSCoordinates ::
  -- | 'latitude'
  Lude.Double ->
  -- | 'longitude'
  Lude.Double ->
  GPSCoordinates
mkGPSCoordinates pLatitude_ pLongitude_ =
  GPSCoordinates' {latitude = pLatitude_, longitude = pLongitude_}

-- | The latitude coordinate of the location.
--
-- /Note:/ Consider using 'latitude' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpscLatitude :: Lens.Lens' GPSCoordinates Lude.Double
gpscLatitude = Lens.lens (latitude :: GPSCoordinates -> Lude.Double) (\s a -> s {latitude = a} :: GPSCoordinates)
{-# DEPRECATED gpscLatitude "Use generic-lens or generic-optics with 'latitude' instead." #-}

-- | The longitude coordinate of the location.
--
-- /Note:/ Consider using 'longitude' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gpscLongitude :: Lens.Lens' GPSCoordinates Lude.Double
gpscLongitude = Lens.lens (longitude :: GPSCoordinates -> Lude.Double) (\s a -> s {longitude = a} :: GPSCoordinates)
{-# DEPRECATED gpscLongitude "Use generic-lens or generic-optics with 'longitude' instead." #-}

instance Lude.FromJSON GPSCoordinates where
  parseJSON =
    Lude.withObject
      "GPSCoordinates"
      ( \x ->
          GPSCoordinates'
            Lude.<$> (x Lude..: "Latitude") Lude.<*> (x Lude..: "Longitude")
      )

instance Lude.ToJSON GPSCoordinates where
  toJSON GPSCoordinates' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Latitude" Lude..= latitude),
            Lude.Just ("Longitude" Lude..= longitude)
          ]
      )
