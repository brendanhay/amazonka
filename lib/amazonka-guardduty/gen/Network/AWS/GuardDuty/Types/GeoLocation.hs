{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.GeoLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.GeoLocation
  ( GeoLocation (..),

    -- * Smart constructor
    mkGeoLocation,

    -- * Lenses
    glLat,
    glLon,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the location of the remote IP address.
--
-- /See:/ 'mkGeoLocation' smart constructor.
data GeoLocation = GeoLocation'
  { -- | The latitude information of the remote IP address.
    lat :: Lude.Maybe Lude.Double,
    -- | The longitude information of the remote IP address.
    lon :: Lude.Maybe Lude.Double
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GeoLocation' with the minimum fields required to make a request.
--
-- * 'lat' - The latitude information of the remote IP address.
-- * 'lon' - The longitude information of the remote IP address.
mkGeoLocation ::
  GeoLocation
mkGeoLocation =
  GeoLocation' {lat = Lude.Nothing, lon = Lude.Nothing}

-- | The latitude information of the remote IP address.
--
-- /Note:/ Consider using 'lat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glLat :: Lens.Lens' GeoLocation (Lude.Maybe Lude.Double)
glLat = Lens.lens (lat :: GeoLocation -> Lude.Maybe Lude.Double) (\s a -> s {lat = a} :: GeoLocation)
{-# DEPRECATED glLat "Use generic-lens or generic-optics with 'lat' instead." #-}

-- | The longitude information of the remote IP address.
--
-- /Note:/ Consider using 'lon' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glLon :: Lens.Lens' GeoLocation (Lude.Maybe Lude.Double)
glLon = Lens.lens (lon :: GeoLocation -> Lude.Maybe Lude.Double) (\s a -> s {lon = a} :: GeoLocation)
{-# DEPRECATED glLon "Use generic-lens or generic-optics with 'lon' instead." #-}

instance Lude.FromJSON GeoLocation where
  parseJSON =
    Lude.withObject
      "GeoLocation"
      ( \x ->
          GeoLocation'
            Lude.<$> (x Lude..:? "lat") Lude.<*> (x Lude..:? "lon")
      )
