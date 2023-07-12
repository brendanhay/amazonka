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
-- Module      : Amazonka.NetworkManager.Types.Location
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.Location where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a location.
--
-- /See:/ 'newLocation' smart constructor.
data Location = Location'
  { -- | The physical address.
    address :: Prelude.Maybe Prelude.Text,
    -- | The latitude.
    latitude :: Prelude.Maybe Prelude.Text,
    -- | The longitude.
    longitude :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Location' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'address', 'location_address' - The physical address.
--
-- 'latitude', 'location_latitude' - The latitude.
--
-- 'longitude', 'location_longitude' - The longitude.
newLocation ::
  Location
newLocation =
  Location'
    { address = Prelude.Nothing,
      latitude = Prelude.Nothing,
      longitude = Prelude.Nothing
    }

-- | The physical address.
location_address :: Lens.Lens' Location (Prelude.Maybe Prelude.Text)
location_address = Lens.lens (\Location' {address} -> address) (\s@Location' {} a -> s {address = a} :: Location)

-- | The latitude.
location_latitude :: Lens.Lens' Location (Prelude.Maybe Prelude.Text)
location_latitude = Lens.lens (\Location' {latitude} -> latitude) (\s@Location' {} a -> s {latitude = a} :: Location)

-- | The longitude.
location_longitude :: Lens.Lens' Location (Prelude.Maybe Prelude.Text)
location_longitude = Lens.lens (\Location' {longitude} -> longitude) (\s@Location' {} a -> s {longitude = a} :: Location)

instance Data.FromJSON Location where
  parseJSON =
    Data.withObject
      "Location"
      ( \x ->
          Location'
            Prelude.<$> (x Data..:? "Address")
            Prelude.<*> (x Data..:? "Latitude")
            Prelude.<*> (x Data..:? "Longitude")
      )

instance Prelude.Hashable Location where
  hashWithSalt _salt Location' {..} =
    _salt
      `Prelude.hashWithSalt` address
      `Prelude.hashWithSalt` latitude
      `Prelude.hashWithSalt` longitude

instance Prelude.NFData Location where
  rnf Location' {..} =
    Prelude.rnf address
      `Prelude.seq` Prelude.rnf latitude
      `Prelude.seq` Prelude.rnf longitude

instance Data.ToJSON Location where
  toJSON Location' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Address" Data..=) Prelude.<$> address,
            ("Latitude" Data..=) Prelude.<$> latitude,
            ("Longitude" Data..=) Prelude.<$> longitude
          ]
      )
