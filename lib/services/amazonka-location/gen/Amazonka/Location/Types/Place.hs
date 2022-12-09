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
-- Module      : Amazonka.Location.Types.Place
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.Place where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types.PlaceGeometry
import Amazonka.Location.Types.TimeZone
import qualified Amazonka.Prelude as Prelude

-- | Contains details about addresses or points of interest that match the
-- search criteria.
--
-- Not all details are included with all responses. Some details may only
-- be returned by specific data partners.
--
-- /See:/ 'newPlace' smart constructor.
data Place = Place'
  { -- | The numerical portion of an address, such as a building number.
    addressNumber :: Prelude.Maybe Prelude.Text,
    -- | A country\/region specified using
    -- <https://www.iso.org/iso-3166-country-codes.html ISO 3166> 3-digit
    -- country\/region code. For example, @CAN@.
    country :: Prelude.Maybe Prelude.Text,
    -- | @True@ if the result is interpolated from other known places.
    --
    -- @False@ if the Place is a known place.
    --
    -- Not returned when the partner does not provide the information.
    --
    -- For example, returns @False@ for an address location that is found in
    -- the partner data, but returns @True@ if an address does not exist in the
    -- partner data and its location is calculated by interpolating between
    -- other known addresses.
    interpolated :: Prelude.Maybe Prelude.Bool,
    -- | The full name and address of the point of interest such as a city,
    -- region, or country. For example, @123 Any Street, Any Town, USA@.
    label :: Prelude.Maybe Prelude.Text,
    -- | A name for a local area, such as a city or town name. For example,
    -- @Toronto@.
    municipality :: Prelude.Maybe Prelude.Text,
    -- | The name of a community district. For example, @Downtown@.
    neighborhood :: Prelude.Maybe Prelude.Text,
    -- | A group of numbers and letters in a country-specific format, which
    -- accompanies the address for the purpose of identifying a location.
    postalCode :: Prelude.Maybe Prelude.Text,
    -- | A name for an area or geographical division, such as a province or state
    -- name. For example, @British Columbia@.
    region :: Prelude.Maybe Prelude.Text,
    -- | The name for a street or a road to identify a location. For example,
    -- @Main Street@.
    street :: Prelude.Maybe Prelude.Text,
    -- | A country, or an area that\'s part of a larger region. For example,
    -- @Metro Vancouver@.
    subRegion :: Prelude.Maybe Prelude.Text,
    -- | The time zone in which the @Place@ is located. Returned only when using
    -- HERE as the selected partner.
    timeZone :: Prelude.Maybe TimeZone,
    -- | For addresses with multiple units, the unit identifier. Can include
    -- numbers and letters, for example @3B@ or @Unit 123@.
    --
    -- Returned only for a place index that uses Esri as a data provider. Is
    -- not returned for @SearchPlaceIndexForPosition@.
    unitNumber :: Prelude.Maybe Prelude.Text,
    -- | For addresses with a @UnitNumber@, the type of unit. For example,
    -- @Apartment@.
    unitType :: Prelude.Maybe Prelude.Text,
    geometry :: PlaceGeometry
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Place' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addressNumber', 'place_addressNumber' - The numerical portion of an address, such as a building number.
--
-- 'country', 'place_country' - A country\/region specified using
-- <https://www.iso.org/iso-3166-country-codes.html ISO 3166> 3-digit
-- country\/region code. For example, @CAN@.
--
-- 'interpolated', 'place_interpolated' - @True@ if the result is interpolated from other known places.
--
-- @False@ if the Place is a known place.
--
-- Not returned when the partner does not provide the information.
--
-- For example, returns @False@ for an address location that is found in
-- the partner data, but returns @True@ if an address does not exist in the
-- partner data and its location is calculated by interpolating between
-- other known addresses.
--
-- 'label', 'place_label' - The full name and address of the point of interest such as a city,
-- region, or country. For example, @123 Any Street, Any Town, USA@.
--
-- 'municipality', 'place_municipality' - A name for a local area, such as a city or town name. For example,
-- @Toronto@.
--
-- 'neighborhood', 'place_neighborhood' - The name of a community district. For example, @Downtown@.
--
-- 'postalCode', 'place_postalCode' - A group of numbers and letters in a country-specific format, which
-- accompanies the address for the purpose of identifying a location.
--
-- 'region', 'place_region' - A name for an area or geographical division, such as a province or state
-- name. For example, @British Columbia@.
--
-- 'street', 'place_street' - The name for a street or a road to identify a location. For example,
-- @Main Street@.
--
-- 'subRegion', 'place_subRegion' - A country, or an area that\'s part of a larger region. For example,
-- @Metro Vancouver@.
--
-- 'timeZone', 'place_timeZone' - The time zone in which the @Place@ is located. Returned only when using
-- HERE as the selected partner.
--
-- 'unitNumber', 'place_unitNumber' - For addresses with multiple units, the unit identifier. Can include
-- numbers and letters, for example @3B@ or @Unit 123@.
--
-- Returned only for a place index that uses Esri as a data provider. Is
-- not returned for @SearchPlaceIndexForPosition@.
--
-- 'unitType', 'place_unitType' - For addresses with a @UnitNumber@, the type of unit. For example,
-- @Apartment@.
--
-- 'geometry', 'place_geometry' - Undocumented member.
newPlace ::
  -- | 'geometry'
  PlaceGeometry ->
  Place
newPlace pGeometry_ =
  Place'
    { addressNumber = Prelude.Nothing,
      country = Prelude.Nothing,
      interpolated = Prelude.Nothing,
      label = Prelude.Nothing,
      municipality = Prelude.Nothing,
      neighborhood = Prelude.Nothing,
      postalCode = Prelude.Nothing,
      region = Prelude.Nothing,
      street = Prelude.Nothing,
      subRegion = Prelude.Nothing,
      timeZone = Prelude.Nothing,
      unitNumber = Prelude.Nothing,
      unitType = Prelude.Nothing,
      geometry = pGeometry_
    }

-- | The numerical portion of an address, such as a building number.
place_addressNumber :: Lens.Lens' Place (Prelude.Maybe Prelude.Text)
place_addressNumber = Lens.lens (\Place' {addressNumber} -> addressNumber) (\s@Place' {} a -> s {addressNumber = a} :: Place)

-- | A country\/region specified using
-- <https://www.iso.org/iso-3166-country-codes.html ISO 3166> 3-digit
-- country\/region code. For example, @CAN@.
place_country :: Lens.Lens' Place (Prelude.Maybe Prelude.Text)
place_country = Lens.lens (\Place' {country} -> country) (\s@Place' {} a -> s {country = a} :: Place)

-- | @True@ if the result is interpolated from other known places.
--
-- @False@ if the Place is a known place.
--
-- Not returned when the partner does not provide the information.
--
-- For example, returns @False@ for an address location that is found in
-- the partner data, but returns @True@ if an address does not exist in the
-- partner data and its location is calculated by interpolating between
-- other known addresses.
place_interpolated :: Lens.Lens' Place (Prelude.Maybe Prelude.Bool)
place_interpolated = Lens.lens (\Place' {interpolated} -> interpolated) (\s@Place' {} a -> s {interpolated = a} :: Place)

-- | The full name and address of the point of interest such as a city,
-- region, or country. For example, @123 Any Street, Any Town, USA@.
place_label :: Lens.Lens' Place (Prelude.Maybe Prelude.Text)
place_label = Lens.lens (\Place' {label} -> label) (\s@Place' {} a -> s {label = a} :: Place)

-- | A name for a local area, such as a city or town name. For example,
-- @Toronto@.
place_municipality :: Lens.Lens' Place (Prelude.Maybe Prelude.Text)
place_municipality = Lens.lens (\Place' {municipality} -> municipality) (\s@Place' {} a -> s {municipality = a} :: Place)

-- | The name of a community district. For example, @Downtown@.
place_neighborhood :: Lens.Lens' Place (Prelude.Maybe Prelude.Text)
place_neighborhood = Lens.lens (\Place' {neighborhood} -> neighborhood) (\s@Place' {} a -> s {neighborhood = a} :: Place)

-- | A group of numbers and letters in a country-specific format, which
-- accompanies the address for the purpose of identifying a location.
place_postalCode :: Lens.Lens' Place (Prelude.Maybe Prelude.Text)
place_postalCode = Lens.lens (\Place' {postalCode} -> postalCode) (\s@Place' {} a -> s {postalCode = a} :: Place)

-- | A name for an area or geographical division, such as a province or state
-- name. For example, @British Columbia@.
place_region :: Lens.Lens' Place (Prelude.Maybe Prelude.Text)
place_region = Lens.lens (\Place' {region} -> region) (\s@Place' {} a -> s {region = a} :: Place)

-- | The name for a street or a road to identify a location. For example,
-- @Main Street@.
place_street :: Lens.Lens' Place (Prelude.Maybe Prelude.Text)
place_street = Lens.lens (\Place' {street} -> street) (\s@Place' {} a -> s {street = a} :: Place)

-- | A country, or an area that\'s part of a larger region. For example,
-- @Metro Vancouver@.
place_subRegion :: Lens.Lens' Place (Prelude.Maybe Prelude.Text)
place_subRegion = Lens.lens (\Place' {subRegion} -> subRegion) (\s@Place' {} a -> s {subRegion = a} :: Place)

-- | The time zone in which the @Place@ is located. Returned only when using
-- HERE as the selected partner.
place_timeZone :: Lens.Lens' Place (Prelude.Maybe TimeZone)
place_timeZone = Lens.lens (\Place' {timeZone} -> timeZone) (\s@Place' {} a -> s {timeZone = a} :: Place)

-- | For addresses with multiple units, the unit identifier. Can include
-- numbers and letters, for example @3B@ or @Unit 123@.
--
-- Returned only for a place index that uses Esri as a data provider. Is
-- not returned for @SearchPlaceIndexForPosition@.
place_unitNumber :: Lens.Lens' Place (Prelude.Maybe Prelude.Text)
place_unitNumber = Lens.lens (\Place' {unitNumber} -> unitNumber) (\s@Place' {} a -> s {unitNumber = a} :: Place)

-- | For addresses with a @UnitNumber@, the type of unit. For example,
-- @Apartment@.
place_unitType :: Lens.Lens' Place (Prelude.Maybe Prelude.Text)
place_unitType = Lens.lens (\Place' {unitType} -> unitType) (\s@Place' {} a -> s {unitType = a} :: Place)

-- | Undocumented member.
place_geometry :: Lens.Lens' Place PlaceGeometry
place_geometry = Lens.lens (\Place' {geometry} -> geometry) (\s@Place' {} a -> s {geometry = a} :: Place)

instance Data.FromJSON Place where
  parseJSON =
    Data.withObject
      "Place"
      ( \x ->
          Place'
            Prelude.<$> (x Data..:? "AddressNumber")
            Prelude.<*> (x Data..:? "Country")
            Prelude.<*> (x Data..:? "Interpolated")
            Prelude.<*> (x Data..:? "Label")
            Prelude.<*> (x Data..:? "Municipality")
            Prelude.<*> (x Data..:? "Neighborhood")
            Prelude.<*> (x Data..:? "PostalCode")
            Prelude.<*> (x Data..:? "Region")
            Prelude.<*> (x Data..:? "Street")
            Prelude.<*> (x Data..:? "SubRegion")
            Prelude.<*> (x Data..:? "TimeZone")
            Prelude.<*> (x Data..:? "UnitNumber")
            Prelude.<*> (x Data..:? "UnitType")
            Prelude.<*> (x Data..: "Geometry")
      )

instance Prelude.Hashable Place where
  hashWithSalt _salt Place' {..} =
    _salt `Prelude.hashWithSalt` addressNumber
      `Prelude.hashWithSalt` country
      `Prelude.hashWithSalt` interpolated
      `Prelude.hashWithSalt` label
      `Prelude.hashWithSalt` municipality
      `Prelude.hashWithSalt` neighborhood
      `Prelude.hashWithSalt` postalCode
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` street
      `Prelude.hashWithSalt` subRegion
      `Prelude.hashWithSalt` timeZone
      `Prelude.hashWithSalt` unitNumber
      `Prelude.hashWithSalt` unitType
      `Prelude.hashWithSalt` geometry

instance Prelude.NFData Place where
  rnf Place' {..} =
    Prelude.rnf addressNumber
      `Prelude.seq` Prelude.rnf country
      `Prelude.seq` Prelude.rnf interpolated
      `Prelude.seq` Prelude.rnf label
      `Prelude.seq` Prelude.rnf municipality
      `Prelude.seq` Prelude.rnf neighborhood
      `Prelude.seq` Prelude.rnf postalCode
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf street
      `Prelude.seq` Prelude.rnf subRegion
      `Prelude.seq` Prelude.rnf timeZone
      `Prelude.seq` Prelude.rnf unitNumber
      `Prelude.seq` Prelude.rnf unitType
      `Prelude.seq` Prelude.rnf geometry
