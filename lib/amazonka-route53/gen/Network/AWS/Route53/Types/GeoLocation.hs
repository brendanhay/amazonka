{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.GeoLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.GeoLocation
  ( GeoLocation (..),

    -- * Smart constructor
    mkGeoLocation,

    -- * Lenses
    glContinentCode,
    glCountryCode,
    glSubdivisionCode,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53.Internal as Types
import qualified Network.AWS.Route53.Types.GeoLocationContinentCode as Types
import qualified Network.AWS.Route53.Types.GeoLocationCountryCode as Types
import qualified Network.AWS.Route53.Types.GeoLocationSubdivisionCode as Types

-- | A complex type that contains information about a geographic location.
--
-- /See:/ 'mkGeoLocation' smart constructor.
data GeoLocation = GeoLocation'
  { -- | The two-letter code for the continent.
    --
    -- Amazon Route 53 supports the following continent codes:
    --
    --     * __AF__ : Africa
    --
    --
    --     * __AN__ : Antarctica
    --
    --
    --     * __AS__ : Asia
    --
    --
    --     * __EU__ : Europe
    --
    --
    --     * __OC__ : Oceania
    --
    --
    --     * __NA__ : North America
    --
    --
    --     * __SA__ : South America
    --
    --
    -- Constraint: Specifying @ContinentCode@ with either @CountryCode@ or @SubdivisionCode@ returns an @InvalidInput@ error.
    continentCode :: Core.Maybe Types.GeoLocationContinentCode,
    -- | For geolocation resource record sets, the two-letter code for a country.
    --
    -- Amazon Route 53 uses the two-letter country codes that are specified in <https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2 ISO standard 3166-1 alpha-2> .
    countryCode :: Core.Maybe Types.GeoLocationCountryCode,
    -- | For geolocation resource record sets, the two-letter code for a state of the United States. Route 53 doesn't support any other values for @SubdivisionCode@ . For a list of state abbreviations, see <https://pe.usps.com/text/pub28/28apb.htm Appendix B: Two–Letter State and Possession Abbreviations> on the United States Postal Service website.
    --
    -- If you specify @subdivisioncode@ , you must also specify @US@ for @CountryCode@ .
    subdivisionCode :: Core.Maybe Types.GeoLocationSubdivisionCode
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GeoLocation' value with any optional fields omitted.
mkGeoLocation ::
  GeoLocation
mkGeoLocation =
  GeoLocation'
    { continentCode = Core.Nothing,
      countryCode = Core.Nothing,
      subdivisionCode = Core.Nothing
    }

-- | The two-letter code for the continent.
--
-- Amazon Route 53 supports the following continent codes:
--
--     * __AF__ : Africa
--
--
--     * __AN__ : Antarctica
--
--
--     * __AS__ : Asia
--
--
--     * __EU__ : Europe
--
--
--     * __OC__ : Oceania
--
--
--     * __NA__ : North America
--
--
--     * __SA__ : South America
--
--
-- Constraint: Specifying @ContinentCode@ with either @CountryCode@ or @SubdivisionCode@ returns an @InvalidInput@ error.
--
-- /Note:/ Consider using 'continentCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glContinentCode :: Lens.Lens' GeoLocation (Core.Maybe Types.GeoLocationContinentCode)
glContinentCode = Lens.field @"continentCode"
{-# DEPRECATED glContinentCode "Use generic-lens or generic-optics with 'continentCode' instead." #-}

-- | For geolocation resource record sets, the two-letter code for a country.
--
-- Amazon Route 53 uses the two-letter country codes that are specified in <https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2 ISO standard 3166-1 alpha-2> .
--
-- /Note:/ Consider using 'countryCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glCountryCode :: Lens.Lens' GeoLocation (Core.Maybe Types.GeoLocationCountryCode)
glCountryCode = Lens.field @"countryCode"
{-# DEPRECATED glCountryCode "Use generic-lens or generic-optics with 'countryCode' instead." #-}

-- | For geolocation resource record sets, the two-letter code for a state of the United States. Route 53 doesn't support any other values for @SubdivisionCode@ . For a list of state abbreviations, see <https://pe.usps.com/text/pub28/28apb.htm Appendix B: Two–Letter State and Possession Abbreviations> on the United States Postal Service website.
--
-- If you specify @subdivisioncode@ , you must also specify @US@ for @CountryCode@ .
--
-- /Note:/ Consider using 'subdivisionCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glSubdivisionCode :: Lens.Lens' GeoLocation (Core.Maybe Types.GeoLocationSubdivisionCode)
glSubdivisionCode = Lens.field @"subdivisionCode"
{-# DEPRECATED glSubdivisionCode "Use generic-lens or generic-optics with 'subdivisionCode' instead." #-}

instance Core.ToXML GeoLocation where
  toXML GeoLocation {..} =
    Core.toXMLNode "ContinentCode" Core.<$> continentCode
      Core.<> Core.toXMLNode "CountryCode" Core.<$> countryCode
      Core.<> Core.toXMLNode "SubdivisionCode" Core.<$> subdivisionCode

instance Core.FromXML GeoLocation where
  parseXML x =
    GeoLocation'
      Core.<$> (x Core..@? "ContinentCode")
      Core.<*> (x Core..@? "CountryCode")
      Core.<*> (x Core..@? "SubdivisionCode")
