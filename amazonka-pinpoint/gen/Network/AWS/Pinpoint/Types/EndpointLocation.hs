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
-- Module      : Network.AWS.Pinpoint.Types.EndpointLocation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EndpointLocation where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies geographic information about an endpoint.
--
-- /See:/ 'newEndpointLocation' smart constructor.
data EndpointLocation = EndpointLocation'
  { -- | The longitude coordinate of the endpoint location, rounded to one
    -- decimal place.
    longitude :: Core.Maybe Core.Double,
    -- | The latitude coordinate of the endpoint location, rounded to one decimal
    -- place.
    latitude :: Core.Maybe Core.Double,
    -- | The postal or ZIP code for the area where the endpoint is located.
    postalCode :: Core.Maybe Core.Text,
    -- | The name of the city where the endpoint is located.
    city :: Core.Maybe Core.Text,
    -- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or
    -- region where the endpoint is located. For example, US for the United
    -- States.
    country :: Core.Maybe Core.Text,
    -- | The name of the region where the endpoint is located. For locations in
    -- the United States, this value is the name of a state.
    region :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EndpointLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'longitude', 'endpointLocation_longitude' - The longitude coordinate of the endpoint location, rounded to one
-- decimal place.
--
-- 'latitude', 'endpointLocation_latitude' - The latitude coordinate of the endpoint location, rounded to one decimal
-- place.
--
-- 'postalCode', 'endpointLocation_postalCode' - The postal or ZIP code for the area where the endpoint is located.
--
-- 'city', 'endpointLocation_city' - The name of the city where the endpoint is located.
--
-- 'country', 'endpointLocation_country' - The two-character code, in ISO 3166-1 alpha-2 format, for the country or
-- region where the endpoint is located. For example, US for the United
-- States.
--
-- 'region', 'endpointLocation_region' - The name of the region where the endpoint is located. For locations in
-- the United States, this value is the name of a state.
newEndpointLocation ::
  EndpointLocation
newEndpointLocation =
  EndpointLocation'
    { longitude = Core.Nothing,
      latitude = Core.Nothing,
      postalCode = Core.Nothing,
      city = Core.Nothing,
      country = Core.Nothing,
      region = Core.Nothing
    }

-- | The longitude coordinate of the endpoint location, rounded to one
-- decimal place.
endpointLocation_longitude :: Lens.Lens' EndpointLocation (Core.Maybe Core.Double)
endpointLocation_longitude = Lens.lens (\EndpointLocation' {longitude} -> longitude) (\s@EndpointLocation' {} a -> s {longitude = a} :: EndpointLocation)

-- | The latitude coordinate of the endpoint location, rounded to one decimal
-- place.
endpointLocation_latitude :: Lens.Lens' EndpointLocation (Core.Maybe Core.Double)
endpointLocation_latitude = Lens.lens (\EndpointLocation' {latitude} -> latitude) (\s@EndpointLocation' {} a -> s {latitude = a} :: EndpointLocation)

-- | The postal or ZIP code for the area where the endpoint is located.
endpointLocation_postalCode :: Lens.Lens' EndpointLocation (Core.Maybe Core.Text)
endpointLocation_postalCode = Lens.lens (\EndpointLocation' {postalCode} -> postalCode) (\s@EndpointLocation' {} a -> s {postalCode = a} :: EndpointLocation)

-- | The name of the city where the endpoint is located.
endpointLocation_city :: Lens.Lens' EndpointLocation (Core.Maybe Core.Text)
endpointLocation_city = Lens.lens (\EndpointLocation' {city} -> city) (\s@EndpointLocation' {} a -> s {city = a} :: EndpointLocation)

-- | The two-character code, in ISO 3166-1 alpha-2 format, for the country or
-- region where the endpoint is located. For example, US for the United
-- States.
endpointLocation_country :: Lens.Lens' EndpointLocation (Core.Maybe Core.Text)
endpointLocation_country = Lens.lens (\EndpointLocation' {country} -> country) (\s@EndpointLocation' {} a -> s {country = a} :: EndpointLocation)

-- | The name of the region where the endpoint is located. For locations in
-- the United States, this value is the name of a state.
endpointLocation_region :: Lens.Lens' EndpointLocation (Core.Maybe Core.Text)
endpointLocation_region = Lens.lens (\EndpointLocation' {region} -> region) (\s@EndpointLocation' {} a -> s {region = a} :: EndpointLocation)

instance Core.FromJSON EndpointLocation where
  parseJSON =
    Core.withObject
      "EndpointLocation"
      ( \x ->
          EndpointLocation'
            Core.<$> (x Core..:? "Longitude")
            Core.<*> (x Core..:? "Latitude")
            Core.<*> (x Core..:? "PostalCode")
            Core.<*> (x Core..:? "City")
            Core.<*> (x Core..:? "Country")
            Core.<*> (x Core..:? "Region")
      )

instance Core.Hashable EndpointLocation

instance Core.NFData EndpointLocation

instance Core.ToJSON EndpointLocation where
  toJSON EndpointLocation' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Longitude" Core..=) Core.<$> longitude,
            ("Latitude" Core..=) Core.<$> latitude,
            ("PostalCode" Core..=) Core.<$> postalCode,
            ("City" Core..=) Core.<$> city,
            ("Country" Core..=) Core.<$> country,
            ("Region" Core..=) Core.<$> region
          ]
      )
