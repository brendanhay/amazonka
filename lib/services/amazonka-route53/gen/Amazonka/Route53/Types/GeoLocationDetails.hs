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
-- Module      : Amazonka.Route53.Types.GeoLocationDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53.Types.GeoLocationDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53.Internal

-- | A complex type that contains the codes and full continent, country, and
-- subdivision names for the specified @geolocation@ code.
--
-- /See:/ 'newGeoLocationDetails' smart constructor.
data GeoLocationDetails = GeoLocationDetails'
  { -- | The code for the subdivision, such as a particular state within the
    -- United States. For a list of US state abbreviations, see
    -- <https://pe.usps.com/text/pub28/28apb.htm Appendix B: Two–Letter State and Possession Abbreviations>
    -- on the United States Postal Service website. For a list of all supported
    -- subdivision codes, use the
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_ListGeoLocations.html ListGeoLocations>
    -- API.
    subdivisionCode :: Prelude.Maybe Prelude.Text,
    -- | The name of the country.
    countryName :: Prelude.Maybe Prelude.Text,
    -- | The two-letter code for the country.
    countryCode :: Prelude.Maybe Prelude.Text,
    -- | The full name of the subdivision. Route 53 currently supports only
    -- states in the United States.
    subdivisionName :: Prelude.Maybe Prelude.Text,
    -- | The two-letter code for the continent.
    continentCode :: Prelude.Maybe Prelude.Text,
    -- | The full name of the continent.
    continentName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GeoLocationDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subdivisionCode', 'geoLocationDetails_subdivisionCode' - The code for the subdivision, such as a particular state within the
-- United States. For a list of US state abbreviations, see
-- <https://pe.usps.com/text/pub28/28apb.htm Appendix B: Two–Letter State and Possession Abbreviations>
-- on the United States Postal Service website. For a list of all supported
-- subdivision codes, use the
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_ListGeoLocations.html ListGeoLocations>
-- API.
--
-- 'countryName', 'geoLocationDetails_countryName' - The name of the country.
--
-- 'countryCode', 'geoLocationDetails_countryCode' - The two-letter code for the country.
--
-- 'subdivisionName', 'geoLocationDetails_subdivisionName' - The full name of the subdivision. Route 53 currently supports only
-- states in the United States.
--
-- 'continentCode', 'geoLocationDetails_continentCode' - The two-letter code for the continent.
--
-- 'continentName', 'geoLocationDetails_continentName' - The full name of the continent.
newGeoLocationDetails ::
  GeoLocationDetails
newGeoLocationDetails =
  GeoLocationDetails'
    { subdivisionCode =
        Prelude.Nothing,
      countryName = Prelude.Nothing,
      countryCode = Prelude.Nothing,
      subdivisionName = Prelude.Nothing,
      continentCode = Prelude.Nothing,
      continentName = Prelude.Nothing
    }

-- | The code for the subdivision, such as a particular state within the
-- United States. For a list of US state abbreviations, see
-- <https://pe.usps.com/text/pub28/28apb.htm Appendix B: Two–Letter State and Possession Abbreviations>
-- on the United States Postal Service website. For a list of all supported
-- subdivision codes, use the
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_ListGeoLocations.html ListGeoLocations>
-- API.
geoLocationDetails_subdivisionCode :: Lens.Lens' GeoLocationDetails (Prelude.Maybe Prelude.Text)
geoLocationDetails_subdivisionCode = Lens.lens (\GeoLocationDetails' {subdivisionCode} -> subdivisionCode) (\s@GeoLocationDetails' {} a -> s {subdivisionCode = a} :: GeoLocationDetails)

-- | The name of the country.
geoLocationDetails_countryName :: Lens.Lens' GeoLocationDetails (Prelude.Maybe Prelude.Text)
geoLocationDetails_countryName = Lens.lens (\GeoLocationDetails' {countryName} -> countryName) (\s@GeoLocationDetails' {} a -> s {countryName = a} :: GeoLocationDetails)

-- | The two-letter code for the country.
geoLocationDetails_countryCode :: Lens.Lens' GeoLocationDetails (Prelude.Maybe Prelude.Text)
geoLocationDetails_countryCode = Lens.lens (\GeoLocationDetails' {countryCode} -> countryCode) (\s@GeoLocationDetails' {} a -> s {countryCode = a} :: GeoLocationDetails)

-- | The full name of the subdivision. Route 53 currently supports only
-- states in the United States.
geoLocationDetails_subdivisionName :: Lens.Lens' GeoLocationDetails (Prelude.Maybe Prelude.Text)
geoLocationDetails_subdivisionName = Lens.lens (\GeoLocationDetails' {subdivisionName} -> subdivisionName) (\s@GeoLocationDetails' {} a -> s {subdivisionName = a} :: GeoLocationDetails)

-- | The two-letter code for the continent.
geoLocationDetails_continentCode :: Lens.Lens' GeoLocationDetails (Prelude.Maybe Prelude.Text)
geoLocationDetails_continentCode = Lens.lens (\GeoLocationDetails' {continentCode} -> continentCode) (\s@GeoLocationDetails' {} a -> s {continentCode = a} :: GeoLocationDetails)

-- | The full name of the continent.
geoLocationDetails_continentName :: Lens.Lens' GeoLocationDetails (Prelude.Maybe Prelude.Text)
geoLocationDetails_continentName = Lens.lens (\GeoLocationDetails' {continentName} -> continentName) (\s@GeoLocationDetails' {} a -> s {continentName = a} :: GeoLocationDetails)

instance Data.FromXML GeoLocationDetails where
  parseXML x =
    GeoLocationDetails'
      Prelude.<$> (x Data..@? "SubdivisionCode")
      Prelude.<*> (x Data..@? "CountryName")
      Prelude.<*> (x Data..@? "CountryCode")
      Prelude.<*> (x Data..@? "SubdivisionName")
      Prelude.<*> (x Data..@? "ContinentCode")
      Prelude.<*> (x Data..@? "ContinentName")

instance Prelude.Hashable GeoLocationDetails where
  hashWithSalt _salt GeoLocationDetails' {..} =
    _salt `Prelude.hashWithSalt` subdivisionCode
      `Prelude.hashWithSalt` countryName
      `Prelude.hashWithSalt` countryCode
      `Prelude.hashWithSalt` subdivisionName
      `Prelude.hashWithSalt` continentCode
      `Prelude.hashWithSalt` continentName

instance Prelude.NFData GeoLocationDetails where
  rnf GeoLocationDetails' {..} =
    Prelude.rnf subdivisionCode
      `Prelude.seq` Prelude.rnf countryName
      `Prelude.seq` Prelude.rnf countryCode
      `Prelude.seq` Prelude.rnf subdivisionName
      `Prelude.seq` Prelude.rnf continentCode
      `Prelude.seq` Prelude.rnf continentName
