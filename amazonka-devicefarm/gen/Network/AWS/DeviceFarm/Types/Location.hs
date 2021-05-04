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
-- Module      : Network.AWS.DeviceFarm.Types.Location
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.Location where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a latitude and longitude pair, expressed in geographic
-- coordinate system degrees (for example, 47.6204, -122.3491).
--
-- Elevation is currently not supported.
--
-- /See:/ 'newLocation' smart constructor.
data Location = Location'
  { -- | The latitude.
    latitude :: Prelude.Double,
    -- | The longitude.
    longitude :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Double ->
  -- | 'longitude'
  Prelude.Double ->
  Location
newLocation pLatitude_ pLongitude_ =
  Location'
    { latitude = pLatitude_,
      longitude = pLongitude_
    }

-- | The latitude.
location_latitude :: Lens.Lens' Location Prelude.Double
location_latitude = Lens.lens (\Location' {latitude} -> latitude) (\s@Location' {} a -> s {latitude = a} :: Location)

-- | The longitude.
location_longitude :: Lens.Lens' Location Prelude.Double
location_longitude = Lens.lens (\Location' {longitude} -> longitude) (\s@Location' {} a -> s {longitude = a} :: Location)

instance Prelude.FromJSON Location where
  parseJSON =
    Prelude.withObject
      "Location"
      ( \x ->
          Location'
            Prelude.<$> (x Prelude..: "latitude")
            Prelude.<*> (x Prelude..: "longitude")
      )

instance Prelude.Hashable Location

instance Prelude.NFData Location

instance Prelude.ToJSON Location where
  toJSON Location' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("latitude" Prelude..= latitude),
            Prelude.Just ("longitude" Prelude..= longitude)
          ]
      )
