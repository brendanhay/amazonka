{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SegmentLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SegmentLocation
  ( SegmentLocation (..),

    -- * Smart constructor
    mkSegmentLocation,

    -- * Lenses
    slCountry,
    slGPSPoint,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.GPSPointDimension
import Network.AWS.Pinpoint.Types.SetDimension
import qualified Network.AWS.Prelude as Lude

-- | Specifies geographical dimension settings for a segment.
--
-- /See:/ 'mkSegmentLocation' smart constructor.
data SegmentLocation = SegmentLocation'
  { -- | The country or region code, in ISO 3166-1 alpha-2 format, for the segment.
    country :: Lude.Maybe SetDimension,
    -- | The GPS location and range for the segment.
    gPSPoint :: Lude.Maybe GPSPointDimension
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SegmentLocation' with the minimum fields required to make a request.
--
-- * 'country' - The country or region code, in ISO 3166-1 alpha-2 format, for the segment.
-- * 'gPSPoint' - The GPS location and range for the segment.
mkSegmentLocation ::
  SegmentLocation
mkSegmentLocation =
  SegmentLocation' {country = Lude.Nothing, gPSPoint = Lude.Nothing}

-- | The country or region code, in ISO 3166-1 alpha-2 format, for the segment.
--
-- /Note:/ Consider using 'country' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slCountry :: Lens.Lens' SegmentLocation (Lude.Maybe SetDimension)
slCountry = Lens.lens (country :: SegmentLocation -> Lude.Maybe SetDimension) (\s a -> s {country = a} :: SegmentLocation)
{-# DEPRECATED slCountry "Use generic-lens or generic-optics with 'country' instead." #-}

-- | The GPS location and range for the segment.
--
-- /Note:/ Consider using 'gPSPoint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
slGPSPoint :: Lens.Lens' SegmentLocation (Lude.Maybe GPSPointDimension)
slGPSPoint = Lens.lens (gPSPoint :: SegmentLocation -> Lude.Maybe GPSPointDimension) (\s a -> s {gPSPoint = a} :: SegmentLocation)
{-# DEPRECATED slGPSPoint "Use generic-lens or generic-optics with 'gPSPoint' instead." #-}

instance Lude.FromJSON SegmentLocation where
  parseJSON =
    Lude.withObject
      "SegmentLocation"
      ( \x ->
          SegmentLocation'
            Lude.<$> (x Lude..:? "Country") Lude.<*> (x Lude..:? "GPSPoint")
      )

instance Lude.ToJSON SegmentLocation where
  toJSON SegmentLocation' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Country" Lude..=) Lude.<$> country,
            ("GPSPoint" Lude..=) Lude.<$> gPSPoint
          ]
      )
