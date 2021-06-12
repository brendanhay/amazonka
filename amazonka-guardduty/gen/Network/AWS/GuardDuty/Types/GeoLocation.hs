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
-- Module      : Network.AWS.GuardDuty.Types.GeoLocation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.GeoLocation where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Contains information about the location of the remote IP address.
--
-- /See:/ 'newGeoLocation' smart constructor.
data GeoLocation = GeoLocation'
  { -- | The latitude information of the remote IP address.
    lat :: Core.Maybe Core.Double,
    -- | The longitude information of the remote IP address.
    lon :: Core.Maybe Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GeoLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lat', 'geoLocation_lat' - The latitude information of the remote IP address.
--
-- 'lon', 'geoLocation_lon' - The longitude information of the remote IP address.
newGeoLocation ::
  GeoLocation
newGeoLocation =
  GeoLocation'
    { lat = Core.Nothing,
      lon = Core.Nothing
    }

-- | The latitude information of the remote IP address.
geoLocation_lat :: Lens.Lens' GeoLocation (Core.Maybe Core.Double)
geoLocation_lat = Lens.lens (\GeoLocation' {lat} -> lat) (\s@GeoLocation' {} a -> s {lat = a} :: GeoLocation)

-- | The longitude information of the remote IP address.
geoLocation_lon :: Lens.Lens' GeoLocation (Core.Maybe Core.Double)
geoLocation_lon = Lens.lens (\GeoLocation' {lon} -> lon) (\s@GeoLocation' {} a -> s {lon = a} :: GeoLocation)

instance Core.FromJSON GeoLocation where
  parseJSON =
    Core.withObject
      "GeoLocation"
      ( \x ->
          GeoLocation'
            Core.<$> (x Core..:? "lat") Core.<*> (x Core..:? "lon")
      )

instance Core.Hashable GeoLocation

instance Core.NFData GeoLocation
