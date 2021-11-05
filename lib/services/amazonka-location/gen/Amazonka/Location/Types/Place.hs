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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.Place where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Location.Types.PlaceGeometry
import qualified Amazonka.Prelude as Prelude

-- | Contains details about addresses or points of interest that match the
-- search criteria.
--
-- /See:/ 'newPlace' smart constructor.
data Place = Place'
  { -- | A name for a local area, such as a city or town name. For example,
    -- @Toronto@.
    municipality :: Prelude.Maybe Prelude.Text,
    -- | The numerical portion of an address, such as a building number.
    addressNumber :: Prelude.Maybe Prelude.Text,
    -- | A group of numbers and letters in a country-specific format, which
    -- accompanies the address for the purpose of identifying a location.
    postalCode :: Prelude.Maybe Prelude.Text,
    -- | A country\/region specified using
    -- <https://www.iso.org/iso-3166-country-codes.html ISO 3166> 3-digit
    -- country\/region code. For example, @CAN@.
    country :: Prelude.Maybe Prelude.Text,
    -- | The name for a street or a road to identify a location. For example,
    -- @Main Street@.
    street :: Prelude.Maybe Prelude.Text,
    -- | A country, or an area that\'s part of a larger region . For example,
    -- @Metro Vancouver@.
    subRegion :: Prelude.Maybe Prelude.Text,
    -- | A name for an area or geographical division, such as a province or state
    -- name. For example, @British Columbia@.
    region :: Prelude.Maybe Prelude.Text,
    -- | The full name and address of the point of interest such as a city,
    -- region, or country. For example, @123 Any Street, Any Town, USA@.
    label :: Prelude.Maybe Prelude.Text,
    -- | The name of a community district. For example, @Downtown@.
    neighborhood :: Prelude.Maybe Prelude.Text,
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
-- 'municipality', 'place_municipality' - A name for a local area, such as a city or town name. For example,
-- @Toronto@.
--
-- 'addressNumber', 'place_addressNumber' - The numerical portion of an address, such as a building number.
--
-- 'postalCode', 'place_postalCode' - A group of numbers and letters in a country-specific format, which
-- accompanies the address for the purpose of identifying a location.
--
-- 'country', 'place_country' - A country\/region specified using
-- <https://www.iso.org/iso-3166-country-codes.html ISO 3166> 3-digit
-- country\/region code. For example, @CAN@.
--
-- 'street', 'place_street' - The name for a street or a road to identify a location. For example,
-- @Main Street@.
--
-- 'subRegion', 'place_subRegion' - A country, or an area that\'s part of a larger region . For example,
-- @Metro Vancouver@.
--
-- 'region', 'place_region' - A name for an area or geographical division, such as a province or state
-- name. For example, @British Columbia@.
--
-- 'label', 'place_label' - The full name and address of the point of interest such as a city,
-- region, or country. For example, @123 Any Street, Any Town, USA@.
--
-- 'neighborhood', 'place_neighborhood' - The name of a community district. For example, @Downtown@.
--
-- 'geometry', 'place_geometry' - Undocumented member.
newPlace ::
  -- | 'geometry'
  PlaceGeometry ->
  Place
newPlace pGeometry_ =
  Place'
    { municipality = Prelude.Nothing,
      addressNumber = Prelude.Nothing,
      postalCode = Prelude.Nothing,
      country = Prelude.Nothing,
      street = Prelude.Nothing,
      subRegion = Prelude.Nothing,
      region = Prelude.Nothing,
      label = Prelude.Nothing,
      neighborhood = Prelude.Nothing,
      geometry = pGeometry_
    }

-- | A name for a local area, such as a city or town name. For example,
-- @Toronto@.
place_municipality :: Lens.Lens' Place (Prelude.Maybe Prelude.Text)
place_municipality = Lens.lens (\Place' {municipality} -> municipality) (\s@Place' {} a -> s {municipality = a} :: Place)

-- | The numerical portion of an address, such as a building number.
place_addressNumber :: Lens.Lens' Place (Prelude.Maybe Prelude.Text)
place_addressNumber = Lens.lens (\Place' {addressNumber} -> addressNumber) (\s@Place' {} a -> s {addressNumber = a} :: Place)

-- | A group of numbers and letters in a country-specific format, which
-- accompanies the address for the purpose of identifying a location.
place_postalCode :: Lens.Lens' Place (Prelude.Maybe Prelude.Text)
place_postalCode = Lens.lens (\Place' {postalCode} -> postalCode) (\s@Place' {} a -> s {postalCode = a} :: Place)

-- | A country\/region specified using
-- <https://www.iso.org/iso-3166-country-codes.html ISO 3166> 3-digit
-- country\/region code. For example, @CAN@.
place_country :: Lens.Lens' Place (Prelude.Maybe Prelude.Text)
place_country = Lens.lens (\Place' {country} -> country) (\s@Place' {} a -> s {country = a} :: Place)

-- | The name for a street or a road to identify a location. For example,
-- @Main Street@.
place_street :: Lens.Lens' Place (Prelude.Maybe Prelude.Text)
place_street = Lens.lens (\Place' {street} -> street) (\s@Place' {} a -> s {street = a} :: Place)

-- | A country, or an area that\'s part of a larger region . For example,
-- @Metro Vancouver@.
place_subRegion :: Lens.Lens' Place (Prelude.Maybe Prelude.Text)
place_subRegion = Lens.lens (\Place' {subRegion} -> subRegion) (\s@Place' {} a -> s {subRegion = a} :: Place)

-- | A name for an area or geographical division, such as a province or state
-- name. For example, @British Columbia@.
place_region :: Lens.Lens' Place (Prelude.Maybe Prelude.Text)
place_region = Lens.lens (\Place' {region} -> region) (\s@Place' {} a -> s {region = a} :: Place)

-- | The full name and address of the point of interest such as a city,
-- region, or country. For example, @123 Any Street, Any Town, USA@.
place_label :: Lens.Lens' Place (Prelude.Maybe Prelude.Text)
place_label = Lens.lens (\Place' {label} -> label) (\s@Place' {} a -> s {label = a} :: Place)

-- | The name of a community district. For example, @Downtown@.
place_neighborhood :: Lens.Lens' Place (Prelude.Maybe Prelude.Text)
place_neighborhood = Lens.lens (\Place' {neighborhood} -> neighborhood) (\s@Place' {} a -> s {neighborhood = a} :: Place)

-- | Undocumented member.
place_geometry :: Lens.Lens' Place PlaceGeometry
place_geometry = Lens.lens (\Place' {geometry} -> geometry) (\s@Place' {} a -> s {geometry = a} :: Place)

instance Core.FromJSON Place where
  parseJSON =
    Core.withObject
      "Place"
      ( \x ->
          Place'
            Prelude.<$> (x Core..:? "Municipality")
            Prelude.<*> (x Core..:? "AddressNumber")
            Prelude.<*> (x Core..:? "PostalCode")
            Prelude.<*> (x Core..:? "Country")
            Prelude.<*> (x Core..:? "Street")
            Prelude.<*> (x Core..:? "SubRegion")
            Prelude.<*> (x Core..:? "Region")
            Prelude.<*> (x Core..:? "Label")
            Prelude.<*> (x Core..:? "Neighborhood")
            Prelude.<*> (x Core..: "Geometry")
      )

instance Prelude.Hashable Place

instance Prelude.NFData Place
