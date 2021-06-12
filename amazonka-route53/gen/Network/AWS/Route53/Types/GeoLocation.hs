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
-- Module      : Network.AWS.Route53.Types.GeoLocation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.GeoLocation where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Route53.Internal

-- | A complex type that contains information about a geographic location.
--
-- /See:/ 'newGeoLocation' smart constructor.
data GeoLocation = GeoLocation'
  { -- | The two-letter code for the continent.
    --
    -- Amazon Route 53 supports the following continent codes:
    --
    -- -   __AF__: Africa
    --
    -- -   __AN__: Antarctica
    --
    -- -   __AS__: Asia
    --
    -- -   __EU__: Europe
    --
    -- -   __OC__: Oceania
    --
    -- -   __NA__: North America
    --
    -- -   __SA__: South America
    --
    -- Constraint: Specifying @ContinentCode@ with either @CountryCode@ or
    -- @SubdivisionCode@ returns an @InvalidInput@ error.
    continentCode :: Core.Maybe Core.Text,
    -- | For geolocation resource record sets, the two-letter code for a state of
    -- the United States. Route 53 doesn\'t support any other values for
    -- @SubdivisionCode@. For a list of state abbreviations, see
    -- <https://pe.usps.com/text/pub28/28apb.htm Appendix B: Two–Letter State and Possession Abbreviations>
    -- on the United States Postal Service website.
    --
    -- If you specify @subdivisioncode@, you must also specify @US@ for
    -- @CountryCode@.
    subdivisionCode :: Core.Maybe Core.Text,
    -- | For geolocation resource record sets, the two-letter code for a country.
    --
    -- Amazon Route 53 uses the two-letter country codes that are specified in
    -- <https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2 ISO standard 3166-1 alpha-2>.
    countryCode :: Core.Maybe Core.Text
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
-- 'continentCode', 'geoLocation_continentCode' - The two-letter code for the continent.
--
-- Amazon Route 53 supports the following continent codes:
--
-- -   __AF__: Africa
--
-- -   __AN__: Antarctica
--
-- -   __AS__: Asia
--
-- -   __EU__: Europe
--
-- -   __OC__: Oceania
--
-- -   __NA__: North America
--
-- -   __SA__: South America
--
-- Constraint: Specifying @ContinentCode@ with either @CountryCode@ or
-- @SubdivisionCode@ returns an @InvalidInput@ error.
--
-- 'subdivisionCode', 'geoLocation_subdivisionCode' - For geolocation resource record sets, the two-letter code for a state of
-- the United States. Route 53 doesn\'t support any other values for
-- @SubdivisionCode@. For a list of state abbreviations, see
-- <https://pe.usps.com/text/pub28/28apb.htm Appendix B: Two–Letter State and Possession Abbreviations>
-- on the United States Postal Service website.
--
-- If you specify @subdivisioncode@, you must also specify @US@ for
-- @CountryCode@.
--
-- 'countryCode', 'geoLocation_countryCode' - For geolocation resource record sets, the two-letter code for a country.
--
-- Amazon Route 53 uses the two-letter country codes that are specified in
-- <https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2 ISO standard 3166-1 alpha-2>.
newGeoLocation ::
  GeoLocation
newGeoLocation =
  GeoLocation'
    { continentCode = Core.Nothing,
      subdivisionCode = Core.Nothing,
      countryCode = Core.Nothing
    }

-- | The two-letter code for the continent.
--
-- Amazon Route 53 supports the following continent codes:
--
-- -   __AF__: Africa
--
-- -   __AN__: Antarctica
--
-- -   __AS__: Asia
--
-- -   __EU__: Europe
--
-- -   __OC__: Oceania
--
-- -   __NA__: North America
--
-- -   __SA__: South America
--
-- Constraint: Specifying @ContinentCode@ with either @CountryCode@ or
-- @SubdivisionCode@ returns an @InvalidInput@ error.
geoLocation_continentCode :: Lens.Lens' GeoLocation (Core.Maybe Core.Text)
geoLocation_continentCode = Lens.lens (\GeoLocation' {continentCode} -> continentCode) (\s@GeoLocation' {} a -> s {continentCode = a} :: GeoLocation)

-- | For geolocation resource record sets, the two-letter code for a state of
-- the United States. Route 53 doesn\'t support any other values for
-- @SubdivisionCode@. For a list of state abbreviations, see
-- <https://pe.usps.com/text/pub28/28apb.htm Appendix B: Two–Letter State and Possession Abbreviations>
-- on the United States Postal Service website.
--
-- If you specify @subdivisioncode@, you must also specify @US@ for
-- @CountryCode@.
geoLocation_subdivisionCode :: Lens.Lens' GeoLocation (Core.Maybe Core.Text)
geoLocation_subdivisionCode = Lens.lens (\GeoLocation' {subdivisionCode} -> subdivisionCode) (\s@GeoLocation' {} a -> s {subdivisionCode = a} :: GeoLocation)

-- | For geolocation resource record sets, the two-letter code for a country.
--
-- Amazon Route 53 uses the two-letter country codes that are specified in
-- <https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2 ISO standard 3166-1 alpha-2>.
geoLocation_countryCode :: Lens.Lens' GeoLocation (Core.Maybe Core.Text)
geoLocation_countryCode = Lens.lens (\GeoLocation' {countryCode} -> countryCode) (\s@GeoLocation' {} a -> s {countryCode = a} :: GeoLocation)

instance Core.FromXML GeoLocation where
  parseXML x =
    GeoLocation'
      Core.<$> (x Core..@? "ContinentCode")
      Core.<*> (x Core..@? "SubdivisionCode")
      Core.<*> (x Core..@? "CountryCode")

instance Core.Hashable GeoLocation

instance Core.NFData GeoLocation

instance Core.ToXML GeoLocation where
  toXML GeoLocation' {..} =
    Core.mconcat
      [ "ContinentCode" Core.@= continentCode,
        "SubdivisionCode" Core.@= subdivisionCode,
        "CountryCode" Core.@= countryCode
      ]
