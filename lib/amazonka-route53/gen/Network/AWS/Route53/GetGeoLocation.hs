{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetGeoLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about whether a specified geographic location is supported for Amazon Route 53 geolocation resource record sets.
--
-- Use the following syntax to determine whether a continent is supported for geolocation:
-- @GET /2013-04-01/geolocation?continentcode=/two-letter abbreviation for a continent/ @
-- Use the following syntax to determine whether a country is supported for geolocation:
-- @GET /2013-04-01/geolocation?countrycode=/two-character country code/ @
-- Use the following syntax to determine whether a subdivision of a country is supported for geolocation:
-- @GET /2013-04-01/geolocation?countrycode=/two-character country code/ &subdivisioncode=/subdivision code/ @
module Network.AWS.Route53.GetGeoLocation
  ( -- * Creating a request
    GetGeoLocation (..),
    mkGetGeoLocation,

    -- ** Request lenses
    gglSubdivisionCode,
    gglCountryCode,
    gglContinentCode,

    -- * Destructuring the response
    GetGeoLocationResponse (..),
    mkGetGeoLocationResponse,

    -- ** Response lenses
    gglrsGeoLocationDetails,
    gglrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | A request for information about whether a specified geographic location is supported for Amazon Route 53 geolocation resource record sets.
--
-- /See:/ 'mkGetGeoLocation' smart constructor.
data GetGeoLocation = GetGeoLocation'
  { -- | For @SubdivisionCode@ , Amazon Route 53 supports only states of the United States. For a list of state abbreviations, see <https://pe.usps.com/text/pub28/28apb.htm Appendix B: Two–Letter State and Possession Abbreviations> on the United States Postal Service website.
    --
    -- If you specify @subdivisioncode@ , you must also specify @US@ for @CountryCode@ .
    subdivisionCode :: Lude.Maybe Lude.Text,
    -- | Amazon Route 53 uses the two-letter country codes that are specified in <https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2 ISO standard 3166-1 alpha-2> .
    countryCode :: Lude.Maybe Lude.Text,
    -- | For geolocation resource record sets, a two-letter abbreviation that identifies a continent. Amazon Route 53 supports the following continent codes:
    --
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
    continentCode :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetGeoLocation' with the minimum fields required to make a request.
--
-- * 'subdivisionCode' - For @SubdivisionCode@ , Amazon Route 53 supports only states of the United States. For a list of state abbreviations, see <https://pe.usps.com/text/pub28/28apb.htm Appendix B: Two–Letter State and Possession Abbreviations> on the United States Postal Service website.
--
-- If you specify @subdivisioncode@ , you must also specify @US@ for @CountryCode@ .
-- * 'countryCode' - Amazon Route 53 uses the two-letter country codes that are specified in <https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2 ISO standard 3166-1 alpha-2> .
-- * 'continentCode' - For geolocation resource record sets, a two-letter abbreviation that identifies a continent. Amazon Route 53 supports the following continent codes:
--
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
mkGetGeoLocation ::
  GetGeoLocation
mkGetGeoLocation =
  GetGeoLocation'
    { subdivisionCode = Lude.Nothing,
      countryCode = Lude.Nothing,
      continentCode = Lude.Nothing
    }

-- | For @SubdivisionCode@ , Amazon Route 53 supports only states of the United States. For a list of state abbreviations, see <https://pe.usps.com/text/pub28/28apb.htm Appendix B: Two–Letter State and Possession Abbreviations> on the United States Postal Service website.
--
-- If you specify @subdivisioncode@ , you must also specify @US@ for @CountryCode@ .
--
-- /Note:/ Consider using 'subdivisionCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gglSubdivisionCode :: Lens.Lens' GetGeoLocation (Lude.Maybe Lude.Text)
gglSubdivisionCode = Lens.lens (subdivisionCode :: GetGeoLocation -> Lude.Maybe Lude.Text) (\s a -> s {subdivisionCode = a} :: GetGeoLocation)
{-# DEPRECATED gglSubdivisionCode "Use generic-lens or generic-optics with 'subdivisionCode' instead." #-}

-- | Amazon Route 53 uses the two-letter country codes that are specified in <https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2 ISO standard 3166-1 alpha-2> .
--
-- /Note:/ Consider using 'countryCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gglCountryCode :: Lens.Lens' GetGeoLocation (Lude.Maybe Lude.Text)
gglCountryCode = Lens.lens (countryCode :: GetGeoLocation -> Lude.Maybe Lude.Text) (\s a -> s {countryCode = a} :: GetGeoLocation)
{-# DEPRECATED gglCountryCode "Use generic-lens or generic-optics with 'countryCode' instead." #-}

-- | For geolocation resource record sets, a two-letter abbreviation that identifies a continent. Amazon Route 53 supports the following continent codes:
--
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
--
-- /Note:/ Consider using 'continentCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gglContinentCode :: Lens.Lens' GetGeoLocation (Lude.Maybe Lude.Text)
gglContinentCode = Lens.lens (continentCode :: GetGeoLocation -> Lude.Maybe Lude.Text) (\s a -> s {continentCode = a} :: GetGeoLocation)
{-# DEPRECATED gglContinentCode "Use generic-lens or generic-optics with 'continentCode' instead." #-}

instance Lude.AWSRequest GetGeoLocation where
  type Rs GetGeoLocation = GetGeoLocationResponse
  request = Req.get route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          GetGeoLocationResponse'
            Lude.<$> (x Lude..@ "GeoLocationDetails")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetGeoLocation where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath GetGeoLocation where
  toPath = Lude.const "/2013-04-01/geolocation"

instance Lude.ToQuery GetGeoLocation where
  toQuery GetGeoLocation' {..} =
    Lude.mconcat
      [ "subdivisioncode" Lude.=: subdivisionCode,
        "countrycode" Lude.=: countryCode,
        "continentcode" Lude.=: continentCode
      ]

-- | A complex type that contains the response information for the specified geolocation code.
--
-- /See:/ 'mkGetGeoLocationResponse' smart constructor.
data GetGeoLocationResponse = GetGeoLocationResponse'
  { -- | A complex type that contains the codes and full continent, country, and subdivision names for the specified geolocation code.
    geoLocationDetails :: GeoLocationDetails,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetGeoLocationResponse' with the minimum fields required to make a request.
--
-- * 'geoLocationDetails' - A complex type that contains the codes and full continent, country, and subdivision names for the specified geolocation code.
-- * 'responseStatus' - The response status code.
mkGetGeoLocationResponse ::
  -- | 'geoLocationDetails'
  GeoLocationDetails ->
  -- | 'responseStatus'
  Lude.Int ->
  GetGeoLocationResponse
mkGetGeoLocationResponse pGeoLocationDetails_ pResponseStatus_ =
  GetGeoLocationResponse'
    { geoLocationDetails =
        pGeoLocationDetails_,
      responseStatus = pResponseStatus_
    }

-- | A complex type that contains the codes and full continent, country, and subdivision names for the specified geolocation code.
--
-- /Note:/ Consider using 'geoLocationDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gglrsGeoLocationDetails :: Lens.Lens' GetGeoLocationResponse GeoLocationDetails
gglrsGeoLocationDetails = Lens.lens (geoLocationDetails :: GetGeoLocationResponse -> GeoLocationDetails) (\s a -> s {geoLocationDetails = a} :: GetGeoLocationResponse)
{-# DEPRECATED gglrsGeoLocationDetails "Use generic-lens or generic-optics with 'geoLocationDetails' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gglrsResponseStatus :: Lens.Lens' GetGeoLocationResponse Lude.Int
gglrsResponseStatus = Lens.lens (responseStatus :: GetGeoLocationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetGeoLocationResponse)
{-# DEPRECATED gglrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
