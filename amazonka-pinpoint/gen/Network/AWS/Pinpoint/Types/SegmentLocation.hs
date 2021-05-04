{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.SegmentLocation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SegmentLocation where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.GPSPointDimension
import Network.AWS.Pinpoint.Types.SetDimension
import qualified Network.AWS.Prelude as Prelude

-- | Specifies geographical dimension settings for a segment.
--
-- /See:/ 'newSegmentLocation' smart constructor.
data SegmentLocation = SegmentLocation'
  { -- | The GPS location and range for the segment.
    gPSPoint :: Prelude.Maybe GPSPointDimension,
    -- | The country or region code, in ISO 3166-1 alpha-2 format, for the
    -- segment.
    country :: Prelude.Maybe SetDimension
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SegmentLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gPSPoint', 'segmentLocation_gPSPoint' - The GPS location and range for the segment.
--
-- 'country', 'segmentLocation_country' - The country or region code, in ISO 3166-1 alpha-2 format, for the
-- segment.
newSegmentLocation ::
  SegmentLocation
newSegmentLocation =
  SegmentLocation'
    { gPSPoint = Prelude.Nothing,
      country = Prelude.Nothing
    }

-- | The GPS location and range for the segment.
segmentLocation_gPSPoint :: Lens.Lens' SegmentLocation (Prelude.Maybe GPSPointDimension)
segmentLocation_gPSPoint = Lens.lens (\SegmentLocation' {gPSPoint} -> gPSPoint) (\s@SegmentLocation' {} a -> s {gPSPoint = a} :: SegmentLocation)

-- | The country or region code, in ISO 3166-1 alpha-2 format, for the
-- segment.
segmentLocation_country :: Lens.Lens' SegmentLocation (Prelude.Maybe SetDimension)
segmentLocation_country = Lens.lens (\SegmentLocation' {country} -> country) (\s@SegmentLocation' {} a -> s {country = a} :: SegmentLocation)

instance Prelude.FromJSON SegmentLocation where
  parseJSON =
    Prelude.withObject
      "SegmentLocation"
      ( \x ->
          SegmentLocation'
            Prelude.<$> (x Prelude..:? "GPSPoint")
            Prelude.<*> (x Prelude..:? "Country")
      )

instance Prelude.Hashable SegmentLocation

instance Prelude.NFData SegmentLocation

instance Prelude.ToJSON SegmentLocation where
  toJSON SegmentLocation' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("GPSPoint" Prelude..=) Prelude.<$> gPSPoint,
            ("Country" Prelude..=) Prelude.<$> country
          ]
      )
