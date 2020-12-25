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
import qualified Network.AWS.Prelude as Core

-- | Contains information about the location of the remote IP address.
--
-- /See:/ 'mkGeoLocation' smart constructor.
data GeoLocation = GeoLocation'
  { -- | The latitude information of the remote IP address.
    lat :: Core.Maybe Core.Double,
    -- | The longitude information of the remote IP address.
    lon :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GeoLocation' value with any optional fields omitted.
mkGeoLocation ::
  GeoLocation
mkGeoLocation =
  GeoLocation' {lat = Core.Nothing, lon = Core.Nothing}

-- | The latitude information of the remote IP address.
--
-- /Note:/ Consider using 'lat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glLat :: Lens.Lens' GeoLocation (Core.Maybe Core.Double)
glLat = Lens.field @"lat"
{-# DEPRECATED glLat "Use generic-lens or generic-optics with 'lat' instead." #-}

-- | The longitude information of the remote IP address.
--
-- /Note:/ Consider using 'lon' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glLon :: Lens.Lens' GeoLocation (Core.Maybe Core.Double)
glLon = Lens.field @"lon"
{-# DEPRECATED glLon "Use generic-lens or generic-optics with 'lon' instead." #-}

instance Core.FromJSON GeoLocation where
  parseJSON =
    Core.withObject "GeoLocation" Core.$
      \x ->
        GeoLocation'
          Core.<$> (x Core..:? "lat") Core.<*> (x Core..:? "lon")
