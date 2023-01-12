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
-- Module      : Amazonka.Pinpoint.Types.EndpointLocation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.EndpointLocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies geographic information about an endpoint.
--
-- /See:/ 'newEndpointLocation' smart constructor.
data EndpointLocation = EndpointLocation'
  { -- | The name of the city where the endpoint is located.
    city :: Prelude.Maybe Prelude.Text,
    -- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or
    -- region where the endpoint is located. For example, US for the United
    -- States.
    country :: Prelude.Maybe Prelude.Text,
    -- | The latitude coordinate of the endpoint location, rounded to one decimal
    -- place.
    latitude :: Prelude.Maybe Prelude.Double,
    -- | The longitude coordinate of the endpoint location, rounded to one
    -- decimal place.
    longitude :: Prelude.Maybe Prelude.Double,
    -- | The postal or ZIP code for the area where the endpoint is located.
    postalCode :: Prelude.Maybe Prelude.Text,
    -- | The name of the region where the endpoint is located. For locations in
    -- the United States, this value is the name of a state.
    region :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EndpointLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'city', 'endpointLocation_city' - The name of the city where the endpoint is located.
--
-- 'country', 'endpointLocation_country' - The two-character code, in ISO 3166-1 alpha-2 format, for the country or
-- region where the endpoint is located. For example, US for the United
-- States.
--
-- 'latitude', 'endpointLocation_latitude' - The latitude coordinate of the endpoint location, rounded to one decimal
-- place.
--
-- 'longitude', 'endpointLocation_longitude' - The longitude coordinate of the endpoint location, rounded to one
-- decimal place.
--
-- 'postalCode', 'endpointLocation_postalCode' - The postal or ZIP code for the area where the endpoint is located.
--
-- 'region', 'endpointLocation_region' - The name of the region where the endpoint is located. For locations in
-- the United States, this value is the name of a state.
newEndpointLocation ::
  EndpointLocation
newEndpointLocation =
  EndpointLocation'
    { city = Prelude.Nothing,
      country = Prelude.Nothing,
      latitude = Prelude.Nothing,
      longitude = Prelude.Nothing,
      postalCode = Prelude.Nothing,
      region = Prelude.Nothing
    }

-- | The name of the city where the endpoint is located.
endpointLocation_city :: Lens.Lens' EndpointLocation (Prelude.Maybe Prelude.Text)
endpointLocation_city = Lens.lens (\EndpointLocation' {city} -> city) (\s@EndpointLocation' {} a -> s {city = a} :: EndpointLocation)

-- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or
-- region where the endpoint is located. For example, US for the United
-- States.
endpointLocation_country :: Lens.Lens' EndpointLocation (Prelude.Maybe Prelude.Text)
endpointLocation_country = Lens.lens (\EndpointLocation' {country} -> country) (\s@EndpointLocation' {} a -> s {country = a} :: EndpointLocation)

-- | The latitude coordinate of the endpoint location, rounded to one decimal
-- place.
endpointLocation_latitude :: Lens.Lens' EndpointLocation (Prelude.Maybe Prelude.Double)
endpointLocation_latitude = Lens.lens (\EndpointLocation' {latitude} -> latitude) (\s@EndpointLocation' {} a -> s {latitude = a} :: EndpointLocation)

-- | The longitude coordinate of the endpoint location, rounded to one
-- decimal place.
endpointLocation_longitude :: Lens.Lens' EndpointLocation (Prelude.Maybe Prelude.Double)
endpointLocation_longitude = Lens.lens (\EndpointLocation' {longitude} -> longitude) (\s@EndpointLocation' {} a -> s {longitude = a} :: EndpointLocation)

-- | The postal or ZIP code for the area where the endpoint is located.
endpointLocation_postalCode :: Lens.Lens' EndpointLocation (Prelude.Maybe Prelude.Text)
endpointLocation_postalCode = Lens.lens (\EndpointLocation' {postalCode} -> postalCode) (\s@EndpointLocation' {} a -> s {postalCode = a} :: EndpointLocation)

-- | The name of the region where the endpoint is located. For locations in
-- the United States, this value is the name of a state.
endpointLocation_region :: Lens.Lens' EndpointLocation (Prelude.Maybe Prelude.Text)
endpointLocation_region = Lens.lens (\EndpointLocation' {region} -> region) (\s@EndpointLocation' {} a -> s {region = a} :: EndpointLocation)

instance Data.FromJSON EndpointLocation where
  parseJSON =
    Data.withObject
      "EndpointLocation"
      ( \x ->
          EndpointLocation'
            Prelude.<$> (x Data..:? "City")
            Prelude.<*> (x Data..:? "Country")
            Prelude.<*> (x Data..:? "Latitude")
            Prelude.<*> (x Data..:? "Longitude")
            Prelude.<*> (x Data..:? "PostalCode")
            Prelude.<*> (x Data..:? "Region")
      )

instance Prelude.Hashable EndpointLocation where
  hashWithSalt _salt EndpointLocation' {..} =
    _salt `Prelude.hashWithSalt` city
      `Prelude.hashWithSalt` country
      `Prelude.hashWithSalt` latitude
      `Prelude.hashWithSalt` longitude
      `Prelude.hashWithSalt` postalCode
      `Prelude.hashWithSalt` region

instance Prelude.NFData EndpointLocation where
  rnf EndpointLocation' {..} =
    Prelude.rnf city
      `Prelude.seq` Prelude.rnf country
      `Prelude.seq` Prelude.rnf latitude
      `Prelude.seq` Prelude.rnf longitude
      `Prelude.seq` Prelude.rnf postalCode
      `Prelude.seq` Prelude.rnf region

instance Data.ToJSON EndpointLocation where
  toJSON EndpointLocation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("City" Data..=) Prelude.<$> city,
            ("Country" Data..=) Prelude.<$> country,
            ("Latitude" Data..=) Prelude.<$> latitude,
            ("Longitude" Data..=) Prelude.<$> longitude,
            ("PostalCode" Data..=) Prelude.<$> postalCode,
            ("Region" Data..=) Prelude.<$> region
          ]
      )
