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
-- Module      : Amazonka.InternetMonitor.Types.ImpactedLocation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.InternetMonitor.Types.ImpactedLocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.InternetMonitor.Types.HealthEventStatus
import Amazonka.InternetMonitor.Types.InternetHealth
import Amazonka.InternetMonitor.Types.NetworkImpairment
import qualified Amazonka.Prelude as Prelude

-- | Information about a location impacted by a health event in Amazon
-- CloudWatch Internet Monitor.
--
-- Geographic regions are hierarchically categorized into country,
-- subdivision, metro and city geographic granularities. The geographic
-- region is identified based on the IP address used at the client
-- locations.
--
-- /See:/ 'newImpactedLocation' smart constructor.
data ImpactedLocation = ImpactedLocation'
  { -- | The cause of the impairment. There are two types of network impairments:
    -- Amazon Web Services network issues or internet issues. Internet issues
    -- are typically a problem with a network provider, like an internet
    -- service provider (ISP).
    causedBy :: Prelude.Maybe NetworkImpairment,
    -- | The name of the city where the health event is located.
    city :: Prelude.Maybe Prelude.Text,
    -- | The country code where the health event is located. The ISO 3166-2 codes
    -- for the country is provided, when available.
    countryCode :: Prelude.Maybe Prelude.Text,
    -- | The calculated health at a specific location.
    internetHealth :: Prelude.Maybe InternetHealth,
    -- | The latitude where the health event is located.
    latitude :: Prelude.Maybe Prelude.Double,
    -- | The longitude where the health event is located.
    longitude :: Prelude.Maybe Prelude.Double,
    -- | The metro area where the health event is located.
    --
    -- Metro indicates a metropolitan region in the United States, such as the
    -- region around New York City. In non-US countries, this is a second-level
    -- subdivision. For example, in the United Kingdom, it could be a county, a
    -- London borough, a unitary authority, council area, and so on.
    metro :: Prelude.Maybe Prelude.Text,
    -- | The service location where the health event is located.
    serviceLocation :: Prelude.Maybe Prelude.Text,
    -- | The subdivision location where the health event is located. The
    -- subdivision usually maps to states in most countries (including the
    -- United States). For United Kingdom, it maps to a country (England,
    -- Scotland, Wales) or province (Northern Ireland).
    subdivision :: Prelude.Maybe Prelude.Text,
    -- | The subdivision code where the health event is located. The ISO 3166-2
    -- codes for country subdivisions is provided, when available.
    subdivisionCode :: Prelude.Maybe Prelude.Text,
    -- | The name of the network at an impacted location.
    aSName :: Prelude.Text,
    -- | The Autonomous System Number (ASN) of the network at an impacted
    -- location.
    aSNumber :: Prelude.Integer,
    -- | The name of the country where the health event is located.
    country :: Prelude.Text,
    -- | The status of the health event at an impacted location.
    status :: HealthEventStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImpactedLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'causedBy', 'impactedLocation_causedBy' - The cause of the impairment. There are two types of network impairments:
-- Amazon Web Services network issues or internet issues. Internet issues
-- are typically a problem with a network provider, like an internet
-- service provider (ISP).
--
-- 'city', 'impactedLocation_city' - The name of the city where the health event is located.
--
-- 'countryCode', 'impactedLocation_countryCode' - The country code where the health event is located. The ISO 3166-2 codes
-- for the country is provided, when available.
--
-- 'internetHealth', 'impactedLocation_internetHealth' - The calculated health at a specific location.
--
-- 'latitude', 'impactedLocation_latitude' - The latitude where the health event is located.
--
-- 'longitude', 'impactedLocation_longitude' - The longitude where the health event is located.
--
-- 'metro', 'impactedLocation_metro' - The metro area where the health event is located.
--
-- Metro indicates a metropolitan region in the United States, such as the
-- region around New York City. In non-US countries, this is a second-level
-- subdivision. For example, in the United Kingdom, it could be a county, a
-- London borough, a unitary authority, council area, and so on.
--
-- 'serviceLocation', 'impactedLocation_serviceLocation' - The service location where the health event is located.
--
-- 'subdivision', 'impactedLocation_subdivision' - The subdivision location where the health event is located. The
-- subdivision usually maps to states in most countries (including the
-- United States). For United Kingdom, it maps to a country (England,
-- Scotland, Wales) or province (Northern Ireland).
--
-- 'subdivisionCode', 'impactedLocation_subdivisionCode' - The subdivision code where the health event is located. The ISO 3166-2
-- codes for country subdivisions is provided, when available.
--
-- 'aSName', 'impactedLocation_aSName' - The name of the network at an impacted location.
--
-- 'aSNumber', 'impactedLocation_aSNumber' - The Autonomous System Number (ASN) of the network at an impacted
-- location.
--
-- 'country', 'impactedLocation_country' - The name of the country where the health event is located.
--
-- 'status', 'impactedLocation_status' - The status of the health event at an impacted location.
newImpactedLocation ::
  -- | 'aSName'
  Prelude.Text ->
  -- | 'aSNumber'
  Prelude.Integer ->
  -- | 'country'
  Prelude.Text ->
  -- | 'status'
  HealthEventStatus ->
  ImpactedLocation
newImpactedLocation
  pASName_
  pASNumber_
  pCountry_
  pStatus_ =
    ImpactedLocation'
      { causedBy = Prelude.Nothing,
        city = Prelude.Nothing,
        countryCode = Prelude.Nothing,
        internetHealth = Prelude.Nothing,
        latitude = Prelude.Nothing,
        longitude = Prelude.Nothing,
        metro = Prelude.Nothing,
        serviceLocation = Prelude.Nothing,
        subdivision = Prelude.Nothing,
        subdivisionCode = Prelude.Nothing,
        aSName = pASName_,
        aSNumber = pASNumber_,
        country = pCountry_,
        status = pStatus_
      }

-- | The cause of the impairment. There are two types of network impairments:
-- Amazon Web Services network issues or internet issues. Internet issues
-- are typically a problem with a network provider, like an internet
-- service provider (ISP).
impactedLocation_causedBy :: Lens.Lens' ImpactedLocation (Prelude.Maybe NetworkImpairment)
impactedLocation_causedBy = Lens.lens (\ImpactedLocation' {causedBy} -> causedBy) (\s@ImpactedLocation' {} a -> s {causedBy = a} :: ImpactedLocation)

-- | The name of the city where the health event is located.
impactedLocation_city :: Lens.Lens' ImpactedLocation (Prelude.Maybe Prelude.Text)
impactedLocation_city = Lens.lens (\ImpactedLocation' {city} -> city) (\s@ImpactedLocation' {} a -> s {city = a} :: ImpactedLocation)

-- | The country code where the health event is located. The ISO 3166-2 codes
-- for the country is provided, when available.
impactedLocation_countryCode :: Lens.Lens' ImpactedLocation (Prelude.Maybe Prelude.Text)
impactedLocation_countryCode = Lens.lens (\ImpactedLocation' {countryCode} -> countryCode) (\s@ImpactedLocation' {} a -> s {countryCode = a} :: ImpactedLocation)

-- | The calculated health at a specific location.
impactedLocation_internetHealth :: Lens.Lens' ImpactedLocation (Prelude.Maybe InternetHealth)
impactedLocation_internetHealth = Lens.lens (\ImpactedLocation' {internetHealth} -> internetHealth) (\s@ImpactedLocation' {} a -> s {internetHealth = a} :: ImpactedLocation)

-- | The latitude where the health event is located.
impactedLocation_latitude :: Lens.Lens' ImpactedLocation (Prelude.Maybe Prelude.Double)
impactedLocation_latitude = Lens.lens (\ImpactedLocation' {latitude} -> latitude) (\s@ImpactedLocation' {} a -> s {latitude = a} :: ImpactedLocation)

-- | The longitude where the health event is located.
impactedLocation_longitude :: Lens.Lens' ImpactedLocation (Prelude.Maybe Prelude.Double)
impactedLocation_longitude = Lens.lens (\ImpactedLocation' {longitude} -> longitude) (\s@ImpactedLocation' {} a -> s {longitude = a} :: ImpactedLocation)

-- | The metro area where the health event is located.
--
-- Metro indicates a metropolitan region in the United States, such as the
-- region around New York City. In non-US countries, this is a second-level
-- subdivision. For example, in the United Kingdom, it could be a county, a
-- London borough, a unitary authority, council area, and so on.
impactedLocation_metro :: Lens.Lens' ImpactedLocation (Prelude.Maybe Prelude.Text)
impactedLocation_metro = Lens.lens (\ImpactedLocation' {metro} -> metro) (\s@ImpactedLocation' {} a -> s {metro = a} :: ImpactedLocation)

-- | The service location where the health event is located.
impactedLocation_serviceLocation :: Lens.Lens' ImpactedLocation (Prelude.Maybe Prelude.Text)
impactedLocation_serviceLocation = Lens.lens (\ImpactedLocation' {serviceLocation} -> serviceLocation) (\s@ImpactedLocation' {} a -> s {serviceLocation = a} :: ImpactedLocation)

-- | The subdivision location where the health event is located. The
-- subdivision usually maps to states in most countries (including the
-- United States). For United Kingdom, it maps to a country (England,
-- Scotland, Wales) or province (Northern Ireland).
impactedLocation_subdivision :: Lens.Lens' ImpactedLocation (Prelude.Maybe Prelude.Text)
impactedLocation_subdivision = Lens.lens (\ImpactedLocation' {subdivision} -> subdivision) (\s@ImpactedLocation' {} a -> s {subdivision = a} :: ImpactedLocation)

-- | The subdivision code where the health event is located. The ISO 3166-2
-- codes for country subdivisions is provided, when available.
impactedLocation_subdivisionCode :: Lens.Lens' ImpactedLocation (Prelude.Maybe Prelude.Text)
impactedLocation_subdivisionCode = Lens.lens (\ImpactedLocation' {subdivisionCode} -> subdivisionCode) (\s@ImpactedLocation' {} a -> s {subdivisionCode = a} :: ImpactedLocation)

-- | The name of the network at an impacted location.
impactedLocation_aSName :: Lens.Lens' ImpactedLocation Prelude.Text
impactedLocation_aSName = Lens.lens (\ImpactedLocation' {aSName} -> aSName) (\s@ImpactedLocation' {} a -> s {aSName = a} :: ImpactedLocation)

-- | The Autonomous System Number (ASN) of the network at an impacted
-- location.
impactedLocation_aSNumber :: Lens.Lens' ImpactedLocation Prelude.Integer
impactedLocation_aSNumber = Lens.lens (\ImpactedLocation' {aSNumber} -> aSNumber) (\s@ImpactedLocation' {} a -> s {aSNumber = a} :: ImpactedLocation)

-- | The name of the country where the health event is located.
impactedLocation_country :: Lens.Lens' ImpactedLocation Prelude.Text
impactedLocation_country = Lens.lens (\ImpactedLocation' {country} -> country) (\s@ImpactedLocation' {} a -> s {country = a} :: ImpactedLocation)

-- | The status of the health event at an impacted location.
impactedLocation_status :: Lens.Lens' ImpactedLocation HealthEventStatus
impactedLocation_status = Lens.lens (\ImpactedLocation' {status} -> status) (\s@ImpactedLocation' {} a -> s {status = a} :: ImpactedLocation)

instance Data.FromJSON ImpactedLocation where
  parseJSON =
    Data.withObject
      "ImpactedLocation"
      ( \x ->
          ImpactedLocation'
            Prelude.<$> (x Data..:? "CausedBy")
            Prelude.<*> (x Data..:? "City")
            Prelude.<*> (x Data..:? "CountryCode")
            Prelude.<*> (x Data..:? "InternetHealth")
            Prelude.<*> (x Data..:? "Latitude")
            Prelude.<*> (x Data..:? "Longitude")
            Prelude.<*> (x Data..:? "Metro")
            Prelude.<*> (x Data..:? "ServiceLocation")
            Prelude.<*> (x Data..:? "Subdivision")
            Prelude.<*> (x Data..:? "SubdivisionCode")
            Prelude.<*> (x Data..: "ASName")
            Prelude.<*> (x Data..: "ASNumber")
            Prelude.<*> (x Data..: "Country")
            Prelude.<*> (x Data..: "Status")
      )

instance Prelude.Hashable ImpactedLocation where
  hashWithSalt _salt ImpactedLocation' {..} =
    _salt
      `Prelude.hashWithSalt` causedBy
      `Prelude.hashWithSalt` city
      `Prelude.hashWithSalt` countryCode
      `Prelude.hashWithSalt` internetHealth
      `Prelude.hashWithSalt` latitude
      `Prelude.hashWithSalt` longitude
      `Prelude.hashWithSalt` metro
      `Prelude.hashWithSalt` serviceLocation
      `Prelude.hashWithSalt` subdivision
      `Prelude.hashWithSalt` subdivisionCode
      `Prelude.hashWithSalt` aSName
      `Prelude.hashWithSalt` aSNumber
      `Prelude.hashWithSalt` country
      `Prelude.hashWithSalt` status

instance Prelude.NFData ImpactedLocation where
  rnf ImpactedLocation' {..} =
    Prelude.rnf causedBy
      `Prelude.seq` Prelude.rnf city
      `Prelude.seq` Prelude.rnf countryCode
      `Prelude.seq` Prelude.rnf internetHealth
      `Prelude.seq` Prelude.rnf latitude
      `Prelude.seq` Prelude.rnf longitude
      `Prelude.seq` Prelude.rnf metro
      `Prelude.seq` Prelude.rnf serviceLocation
      `Prelude.seq` Prelude.rnf subdivision
      `Prelude.seq` Prelude.rnf subdivisionCode
      `Prelude.seq` Prelude.rnf aSName
      `Prelude.seq` Prelude.rnf aSNumber
      `Prelude.seq` Prelude.rnf country
      `Prelude.seq` Prelude.rnf status
