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
-- Module      : Network.AWS.Pinpoint.Types.GPSCoordinates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.GPSCoordinates where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies the GPS coordinates of a location.
--
-- /See:/ 'newGPSCoordinates' smart constructor.
data GPSCoordinates = GPSCoordinates'
  { -- | The latitude coordinate of the location.
    latitude :: Core.Double,
    -- | The longitude coordinate of the location.
    longitude :: Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GPSCoordinates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'latitude', 'gPSCoordinates_latitude' - The latitude coordinate of the location.
--
-- 'longitude', 'gPSCoordinates_longitude' - The longitude coordinate of the location.
newGPSCoordinates ::
  -- | 'latitude'
  Core.Double ->
  -- | 'longitude'
  Core.Double ->
  GPSCoordinates
newGPSCoordinates pLatitude_ pLongitude_ =
  GPSCoordinates'
    { latitude = pLatitude_,
      longitude = pLongitude_
    }

-- | The latitude coordinate of the location.
gPSCoordinates_latitude :: Lens.Lens' GPSCoordinates Core.Double
gPSCoordinates_latitude = Lens.lens (\GPSCoordinates' {latitude} -> latitude) (\s@GPSCoordinates' {} a -> s {latitude = a} :: GPSCoordinates)

-- | The longitude coordinate of the location.
gPSCoordinates_longitude :: Lens.Lens' GPSCoordinates Core.Double
gPSCoordinates_longitude = Lens.lens (\GPSCoordinates' {longitude} -> longitude) (\s@GPSCoordinates' {} a -> s {longitude = a} :: GPSCoordinates)

instance Core.FromJSON GPSCoordinates where
  parseJSON =
    Core.withObject
      "GPSCoordinates"
      ( \x ->
          GPSCoordinates'
            Core.<$> (x Core..: "Latitude")
            Core.<*> (x Core..: "Longitude")
      )

instance Core.Hashable GPSCoordinates

instance Core.NFData GPSCoordinates

instance Core.ToJSON GPSCoordinates where
  toJSON GPSCoordinates' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Latitude" Core..= latitude),
            Core.Just ("Longitude" Core..= longitude)
          ]
      )
