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
-- Module      : Network.AWS.DeviceFarm.Types.Location
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.Location where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents a latitude and longitude pair, expressed in geographic
-- coordinate system degrees (for example, 47.6204, -122.3491).
--
-- Elevation is currently not supported.
--
-- /See:/ 'newLocation' smart constructor.
data Location = Location'
  { -- | The latitude.
    latitude :: Core.Double,
    -- | The longitude.
    longitude :: Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Location' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'latitude', 'location_latitude' - The latitude.
--
-- 'longitude', 'location_longitude' - The longitude.
newLocation ::
  -- | 'latitude'
  Core.Double ->
  -- | 'longitude'
  Core.Double ->
  Location
newLocation pLatitude_ pLongitude_ =
  Location'
    { latitude = pLatitude_,
      longitude = pLongitude_
    }

-- | The latitude.
location_latitude :: Lens.Lens' Location Core.Double
location_latitude = Lens.lens (\Location' {latitude} -> latitude) (\s@Location' {} a -> s {latitude = a} :: Location)

-- | The longitude.
location_longitude :: Lens.Lens' Location Core.Double
location_longitude = Lens.lens (\Location' {longitude} -> longitude) (\s@Location' {} a -> s {longitude = a} :: Location)

instance Core.FromJSON Location where
  parseJSON =
    Core.withObject
      "Location"
      ( \x ->
          Location'
            Core.<$> (x Core..: "latitude")
            Core.<*> (x Core..: "longitude")
      )

instance Core.Hashable Location

instance Core.NFData Location

instance Core.ToJSON Location where
  toJSON Location' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("latitude" Core..= latitude),
            Core.Just ("longitude" Core..= longitude)
          ]
      )
