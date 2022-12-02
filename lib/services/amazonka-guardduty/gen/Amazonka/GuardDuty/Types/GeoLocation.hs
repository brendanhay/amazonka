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
-- Module      : Amazonka.GuardDuty.Types.GeoLocation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.GeoLocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the location of the remote IP address.
--
-- /See:/ 'newGeoLocation' smart constructor.
data GeoLocation = GeoLocation'
  { -- | The latitude information of the remote IP address.
    lat :: Prelude.Maybe Prelude.Double,
    -- | The longitude information of the remote IP address.
    lon :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { lat = Prelude.Nothing,
      lon = Prelude.Nothing
    }

-- | The latitude information of the remote IP address.
geoLocation_lat :: Lens.Lens' GeoLocation (Prelude.Maybe Prelude.Double)
geoLocation_lat = Lens.lens (\GeoLocation' {lat} -> lat) (\s@GeoLocation' {} a -> s {lat = a} :: GeoLocation)

-- | The longitude information of the remote IP address.
geoLocation_lon :: Lens.Lens' GeoLocation (Prelude.Maybe Prelude.Double)
geoLocation_lon = Lens.lens (\GeoLocation' {lon} -> lon) (\s@GeoLocation' {} a -> s {lon = a} :: GeoLocation)

instance Data.FromJSON GeoLocation where
  parseJSON =
    Data.withObject
      "GeoLocation"
      ( \x ->
          GeoLocation'
            Prelude.<$> (x Data..:? "lat") Prelude.<*> (x Data..:? "lon")
      )

instance Prelude.Hashable GeoLocation where
  hashWithSalt _salt GeoLocation' {..} =
    _salt `Prelude.hashWithSalt` lat
      `Prelude.hashWithSalt` lon

instance Prelude.NFData GeoLocation where
  rnf GeoLocation' {..} =
    Prelude.rnf lat `Prelude.seq` Prelude.rnf lon
