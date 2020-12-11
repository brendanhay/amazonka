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
    glSubdivisionCode,
    glCountryCode,
    glContinentCode,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53.Internal

-- | A complex type that contains information about a geographic location.
--
-- /See:/ 'mkGeoLocation' smart constructor.
data GeoLocation = GeoLocation'
  { subdivisionCode ::
      Lude.Maybe Lude.Text,
    countryCode :: Lude.Maybe Lude.Text,
    continentCode :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GeoLocation' with the minimum fields required to make a request.
--
-- * 'continentCode' - The two-letter code for the continent.
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
-- * 'countryCode' - For geolocation resource record sets, the two-letter code for a country.
--
-- Amazon Route 53 uses the two-letter country codes that are specified in <https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2 ISO standard 3166-1 alpha-2> .
-- * 'subdivisionCode' - For geolocation resource record sets, the two-letter code for a state of the United States. Route 53 doesn't support any other values for @SubdivisionCode@ . For a list of state abbreviations, see <https://pe.usps.com/text/pub28/28apb.htm Appendix B: Two–Letter State and Possession Abbreviations> on the United States Postal Service website.
--
-- If you specify @subdivisioncode@ , you must also specify @US@ for @CountryCode@ .
mkGeoLocation ::
  GeoLocation
mkGeoLocation =
  GeoLocation'
    { subdivisionCode = Lude.Nothing,
      countryCode = Lude.Nothing,
      continentCode = Lude.Nothing
    }

-- | For geolocation resource record sets, the two-letter code for a state of the United States. Route 53 doesn't support any other values for @SubdivisionCode@ . For a list of state abbreviations, see <https://pe.usps.com/text/pub28/28apb.htm Appendix B: Two–Letter State and Possession Abbreviations> on the United States Postal Service website.
--
-- If you specify @subdivisioncode@ , you must also specify @US@ for @CountryCode@ .
--
-- /Note:/ Consider using 'subdivisionCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glSubdivisionCode :: Lens.Lens' GeoLocation (Lude.Maybe Lude.Text)
glSubdivisionCode = Lens.lens (subdivisionCode :: GeoLocation -> Lude.Maybe Lude.Text) (\s a -> s {subdivisionCode = a} :: GeoLocation)
{-# DEPRECATED glSubdivisionCode "Use generic-lens or generic-optics with 'subdivisionCode' instead." #-}

-- | For geolocation resource record sets, the two-letter code for a country.
--
-- Amazon Route 53 uses the two-letter country codes that are specified in <https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2 ISO standard 3166-1 alpha-2> .
--
-- /Note:/ Consider using 'countryCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glCountryCode :: Lens.Lens' GeoLocation (Lude.Maybe Lude.Text)
glCountryCode = Lens.lens (countryCode :: GeoLocation -> Lude.Maybe Lude.Text) (\s a -> s {countryCode = a} :: GeoLocation)
{-# DEPRECATED glCountryCode "Use generic-lens or generic-optics with 'countryCode' instead." #-}

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
glContinentCode :: Lens.Lens' GeoLocation (Lude.Maybe Lude.Text)
glContinentCode = Lens.lens (continentCode :: GeoLocation -> Lude.Maybe Lude.Text) (\s a -> s {continentCode = a} :: GeoLocation)
{-# DEPRECATED glContinentCode "Use generic-lens or generic-optics with 'continentCode' instead." #-}

instance Lude.FromXML GeoLocation where
  parseXML x =
    GeoLocation'
      Lude.<$> (x Lude..@? "SubdivisionCode")
      Lude.<*> (x Lude..@? "CountryCode")
      Lude.<*> (x Lude..@? "ContinentCode")

instance Lude.ToXML GeoLocation where
  toXML GeoLocation' {..} =
    Lude.mconcat
      [ "SubdivisionCode" Lude.@= subdivisionCode,
        "CountryCode" Lude.@= countryCode,
        "ContinentCode" Lude.@= continentCode
      ]
