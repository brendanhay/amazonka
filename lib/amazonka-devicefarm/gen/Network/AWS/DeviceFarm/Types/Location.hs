-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.Location
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.Location
  ( Location (..),

    -- * Smart constructor
    mkLocation,

    -- * Lenses
    lLatitude,
    lLongitude,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a latitude and longitude pair, expressed in geographic coordinate system degrees (for example, 47.6204, -122.3491).
--
-- Elevation is currently not supported.
--
-- /See:/ 'mkLocation' smart constructor.
data Location = Location'
  { latitude :: Lude.Double,
    longitude :: Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Location' with the minimum fields required to make a request.
--
-- * 'latitude' - The latitude.
-- * 'longitude' - The longitude.
mkLocation ::
  -- | 'latitude'
  Lude.Double ->
  -- | 'longitude'
  Lude.Double ->
  Location
mkLocation pLatitude_ pLongitude_ =
  Location' {latitude = pLatitude_, longitude = pLongitude_}

-- | The latitude.
--
-- /Note:/ Consider using 'latitude' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lLatitude :: Lens.Lens' Location Lude.Double
lLatitude = Lens.lens (latitude :: Location -> Lude.Double) (\s a -> s {latitude = a} :: Location)
{-# DEPRECATED lLatitude "Use generic-lens or generic-optics with 'latitude' instead." #-}

-- | The longitude.
--
-- /Note:/ Consider using 'longitude' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lLongitude :: Lens.Lens' Location Lude.Double
lLongitude = Lens.lens (longitude :: Location -> Lude.Double) (\s a -> s {longitude = a} :: Location)
{-# DEPRECATED lLongitude "Use generic-lens or generic-optics with 'longitude' instead." #-}

instance Lude.FromJSON Location where
  parseJSON =
    Lude.withObject
      "Location"
      ( \x ->
          Location'
            Lude.<$> (x Lude..: "latitude") Lude.<*> (x Lude..: "longitude")
      )

instance Lude.ToJSON Location where
  toJSON Location' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("latitude" Lude..= latitude),
            Lude.Just ("longitude" Lude..= longitude)
          ]
      )
