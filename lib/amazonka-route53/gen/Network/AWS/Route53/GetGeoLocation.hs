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
    gglContinentCode,
    gglCountryCode,
    gglSubdivisionCode,

    -- * Destructuring the response
    GetGeoLocationResponse (..),
    mkGetGeoLocationResponse,

    -- ** Response lenses
    gglrrsGeoLocationDetails,
    gglrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | A request for information about whether a specified geographic location is supported for Amazon Route 53 geolocation resource record sets.
--
-- /See:/ 'mkGetGeoLocation' smart constructor.
data GetGeoLocation = GetGeoLocation'
  { -- | For geolocation resource record sets, a two-letter abbreviation that identifies a continent. Amazon Route 53 supports the following continent codes:
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
    continentCode :: Core.Maybe Types.GeoLocationContinentCode,
    -- | Amazon Route 53 uses the two-letter country codes that are specified in <https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2 ISO standard 3166-1 alpha-2> .
    countryCode :: Core.Maybe Types.GeoLocationCountryCode,
    -- | For @SubdivisionCode@ , Amazon Route 53 supports only states of the United States. For a list of state abbreviations, see <https://pe.usps.com/text/pub28/28apb.htm Appendix B: Two–Letter State and Possession Abbreviations> on the United States Postal Service website.
    --
    -- If you specify @subdivisioncode@ , you must also specify @US@ for @CountryCode@ .
    subdivisionCode :: Core.Maybe Types.SubdivisionCode
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetGeoLocation' value with any optional fields omitted.
mkGetGeoLocation ::
  GetGeoLocation
mkGetGeoLocation =
  GetGeoLocation'
    { continentCode = Core.Nothing,
      countryCode = Core.Nothing,
      subdivisionCode = Core.Nothing
    }

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
gglContinentCode :: Lens.Lens' GetGeoLocation (Core.Maybe Types.GeoLocationContinentCode)
gglContinentCode = Lens.field @"continentCode"
{-# DEPRECATED gglContinentCode "Use generic-lens or generic-optics with 'continentCode' instead." #-}

-- | Amazon Route 53 uses the two-letter country codes that are specified in <https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2 ISO standard 3166-1 alpha-2> .
--
-- /Note:/ Consider using 'countryCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gglCountryCode :: Lens.Lens' GetGeoLocation (Core.Maybe Types.GeoLocationCountryCode)
gglCountryCode = Lens.field @"countryCode"
{-# DEPRECATED gglCountryCode "Use generic-lens or generic-optics with 'countryCode' instead." #-}

-- | For @SubdivisionCode@ , Amazon Route 53 supports only states of the United States. For a list of state abbreviations, see <https://pe.usps.com/text/pub28/28apb.htm Appendix B: Two–Letter State and Possession Abbreviations> on the United States Postal Service website.
--
-- If you specify @subdivisioncode@ , you must also specify @US@ for @CountryCode@ .
--
-- /Note:/ Consider using 'subdivisionCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gglSubdivisionCode :: Lens.Lens' GetGeoLocation (Core.Maybe Types.SubdivisionCode)
gglSubdivisionCode = Lens.field @"subdivisionCode"
{-# DEPRECATED gglSubdivisionCode "Use generic-lens or generic-optics with 'subdivisionCode' instead." #-}

instance Core.AWSRequest GetGeoLocation where
  type Rs GetGeoLocation = GetGeoLocationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/2013-04-01/geolocation",
        Core._rqQuery =
          Core.toQueryValue "continentcode" Core.<$> continentCode
            Core.<> (Core.toQueryValue "countrycode" Core.<$> countryCode)
            Core.<> (Core.toQueryValue "subdivisioncode" Core.<$> subdivisionCode),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          GetGeoLocationResponse'
            Core.<$> (x Core..@ "GeoLocationDetails")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | A complex type that contains the response information for the specified geolocation code.
--
-- /See:/ 'mkGetGeoLocationResponse' smart constructor.
data GetGeoLocationResponse = GetGeoLocationResponse'
  { -- | A complex type that contains the codes and full continent, country, and subdivision names for the specified geolocation code.
    geoLocationDetails :: Types.GeoLocationDetails,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetGeoLocationResponse' value with any optional fields omitted.
mkGetGeoLocationResponse ::
  -- | 'geoLocationDetails'
  Types.GeoLocationDetails ->
  -- | 'responseStatus'
  Core.Int ->
  GetGeoLocationResponse
mkGetGeoLocationResponse geoLocationDetails responseStatus =
  GetGeoLocationResponse' {geoLocationDetails, responseStatus}

-- | A complex type that contains the codes and full continent, country, and subdivision names for the specified geolocation code.
--
-- /Note:/ Consider using 'geoLocationDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gglrrsGeoLocationDetails :: Lens.Lens' GetGeoLocationResponse Types.GeoLocationDetails
gglrrsGeoLocationDetails = Lens.field @"geoLocationDetails"
{-# DEPRECATED gglrrsGeoLocationDetails "Use generic-lens or generic-optics with 'geoLocationDetails' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gglrrsResponseStatus :: Lens.Lens' GetGeoLocationResponse Core.Int
gglrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gglrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
