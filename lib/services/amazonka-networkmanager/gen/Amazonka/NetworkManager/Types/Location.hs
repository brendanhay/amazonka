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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.Location where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a location.
--
-- /See:/ 'newLocation' smart constructor.
data Location = Location'
  { -- | The latitude.
    latitude :: Prelude.Maybe Prelude.Text,
    -- | The physical address.
    address :: Prelude.Maybe Prelude.Text,
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
-- 'latitude', 'location_latitude' - The latitude.
--
-- 'address', 'location_address' - The physical address.
--
-- 'longitude', 'location_longitude' - The longitude.
newLocation ::
  Location
newLocation =
  Location'
    { latitude = Prelude.Nothing,
      address = Prelude.Nothing,
      longitude = Prelude.Nothing
    }

-- | The latitude.
location_latitude :: Lens.Lens' Location (Prelude.Maybe Prelude.Text)
location_latitude = Lens.lens (\Location' {latitude} -> latitude) (\s@Location' {} a -> s {latitude = a} :: Location)

-- | The physical address.
location_address :: Lens.Lens' Location (Prelude.Maybe Prelude.Text)
location_address = Lens.lens (\Location' {address} -> address) (\s@Location' {} a -> s {address = a} :: Location)

-- | The longitude.
location_longitude :: Lens.Lens' Location (Prelude.Maybe Prelude.Text)
location_longitude = Lens.lens (\Location' {longitude} -> longitude) (\s@Location' {} a -> s {longitude = a} :: Location)

instance Core.FromJSON Location where
  parseJSON =
    Core.withObject
      "Location"
      ( \x ->
          Location'
            Prelude.<$> (x Core..:? "Latitude")
            Prelude.<*> (x Core..:? "Address")
            Prelude.<*> (x Core..:? "Longitude")
      )

instance Prelude.Hashable Location where
  hashWithSalt salt' Location' {..} =
    salt' `Prelude.hashWithSalt` longitude
      `Prelude.hashWithSalt` address
      `Prelude.hashWithSalt` latitude

instance Prelude.NFData Location where
  rnf Location' {..} =
    Prelude.rnf latitude
      `Prelude.seq` Prelude.rnf longitude
      `Prelude.seq` Prelude.rnf address

instance Core.ToJSON Location where
  toJSON Location' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Latitude" Core..=) Prelude.<$> latitude,
            ("Address" Core..=) Prelude.<$> address,
            ("Longitude" Core..=) Prelude.<$> longitude
          ]
      )
