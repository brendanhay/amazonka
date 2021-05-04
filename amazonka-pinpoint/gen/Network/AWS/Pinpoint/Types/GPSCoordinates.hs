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
-- Module      : Network.AWS.Pinpoint.Types.GPSCoordinates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.GPSCoordinates where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the GPS coordinates of a location.
--
-- /See:/ 'newGPSCoordinates' smart constructor.
data GPSCoordinates = GPSCoordinates'
  { -- | The latitude coordinate of the location.
    latitude :: Prelude.Double,
    -- | The longitude coordinate of the location.
    longitude :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Double ->
  -- | 'longitude'
  Prelude.Double ->
  GPSCoordinates
newGPSCoordinates pLatitude_ pLongitude_ =
  GPSCoordinates'
    { latitude = pLatitude_,
      longitude = pLongitude_
    }

-- | The latitude coordinate of the location.
gPSCoordinates_latitude :: Lens.Lens' GPSCoordinates Prelude.Double
gPSCoordinates_latitude = Lens.lens (\GPSCoordinates' {latitude} -> latitude) (\s@GPSCoordinates' {} a -> s {latitude = a} :: GPSCoordinates)

-- | The longitude coordinate of the location.
gPSCoordinates_longitude :: Lens.Lens' GPSCoordinates Prelude.Double
gPSCoordinates_longitude = Lens.lens (\GPSCoordinates' {longitude} -> longitude) (\s@GPSCoordinates' {} a -> s {longitude = a} :: GPSCoordinates)

instance Prelude.FromJSON GPSCoordinates where
  parseJSON =
    Prelude.withObject
      "GPSCoordinates"
      ( \x ->
          GPSCoordinates'
            Prelude.<$> (x Prelude..: "Latitude")
            Prelude.<*> (x Prelude..: "Longitude")
      )

instance Prelude.Hashable GPSCoordinates

instance Prelude.NFData GPSCoordinates

instance Prelude.ToJSON GPSCoordinates where
  toJSON GPSCoordinates' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Latitude" Prelude..= latitude),
            Prelude.Just ("Longitude" Prelude..= longitude)
          ]
      )
