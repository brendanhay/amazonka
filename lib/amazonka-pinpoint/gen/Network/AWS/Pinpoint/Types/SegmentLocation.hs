{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SegmentLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Pinpoint.Types.SegmentLocation
  ( SegmentLocation (..)
  -- * Smart constructor
  , mkSegmentLocation
  -- * Lenses
  , slCountry
  , slGPSPoint
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pinpoint.Types.GPSPointDimension as Types
import qualified Network.AWS.Pinpoint.Types.SetDimension as Types
import qualified Network.AWS.Prelude as Core

-- | Specifies geographical dimension settings for a segment.
--
-- /See:/ 'mkSegmentLocation' smart constructor.
data SegmentLocation = SegmentLocation'
  { country :: Core.Maybe Types.SetDimension
    -- ^ The country or region code, in ISO 3166-1 alpha-2 format, for the segment.
  , gPSPoint :: Core.Maybe Types.GPSPointDimension
    -- ^ The GPS location and range for the segment.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SegmentLocation' value with any optional fields omitted.
mkSegmentLocation
    :: SegmentLocation
mkSegmentLocation
  = SegmentLocation'{country = Core.Nothing, gPSPoint = Core.Nothing}

-- | The country or region code, in ISO 3166-1 alpha-2 format, for the segment.
--
-- /Note:/ Consider using 'country' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slCountry :: Lens.Lens' SegmentLocation (Core.Maybe Types.SetDimension)
slCountry = Lens.field @"country"
{-# INLINEABLE slCountry #-}
{-# DEPRECATED country "Use generic-lens or generic-optics with 'country' instead"  #-}

-- | The GPS location and range for the segment.
--
-- /Note:/ Consider using 'gPSPoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slGPSPoint :: Lens.Lens' SegmentLocation (Core.Maybe Types.GPSPointDimension)
slGPSPoint = Lens.field @"gPSPoint"
{-# INLINEABLE slGPSPoint #-}
{-# DEPRECATED gPSPoint "Use generic-lens or generic-optics with 'gPSPoint' instead"  #-}

instance Core.FromJSON SegmentLocation where
        toJSON SegmentLocation{..}
          = Core.object
              (Core.catMaybes
                 [("Country" Core..=) Core.<$> country,
                  ("GPSPoint" Core..=) Core.<$> gPSPoint])

instance Core.FromJSON SegmentLocation where
        parseJSON
          = Core.withObject "SegmentLocation" Core.$
              \ x ->
                SegmentLocation' Core.<$>
                  (x Core..:? "Country") Core.<*> x Core..:? "GPSPoint"
