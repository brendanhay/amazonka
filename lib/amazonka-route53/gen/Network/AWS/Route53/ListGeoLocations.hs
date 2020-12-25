{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.ListGeoLocations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of supported geographic locations.
--
-- Countries are listed first, and continents are listed last. If Amazon Route 53 supports subdivisions for a country (for example, states or provinces), the subdivisions for that country are listed in alphabetical order immediately after the corresponding country.
-- For a list of supported geolocation codes, see the <https://docs.aws.amazon.com/Route53/latest/APIReference/API_GeoLocation.html GeoLocation> data type.
module Network.AWS.Route53.ListGeoLocations
  ( -- * Creating a request
    ListGeoLocations (..),
    mkListGeoLocations,

    -- ** Request lenses
    lglMaxItems,
    lglStartContinentCode,
    lglStartCountryCode,
    lglStartSubdivisionCode,

    -- * Destructuring the response
    ListGeoLocationsResponse (..),
    mkListGeoLocationsResponse,

    -- ** Response lenses
    lglrrsGeoLocationDetailsList,
    lglrrsIsTruncated,
    lglrrsMaxItems,
    lglrrsNextContinentCode,
    lglrrsNextCountryCode,
    lglrrsNextSubdivisionCode,
    lglrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53.Types as Types

-- | A request to get a list of geographic locations that Amazon Route 53 supports for geolocation resource record sets.
--
-- /See:/ 'mkListGeoLocations' smart constructor.
data ListGeoLocations = ListGeoLocations'
  { -- | (Optional) The maximum number of geolocations to be included in the response body for this request. If more than @maxitems@ geolocations remain to be listed, then the value of the @IsTruncated@ element in the response is @true@ .
    maxItems :: Core.Maybe Types.MaxItems,
    -- | The code for the continent with which you want to start listing locations that Amazon Route 53 supports for geolocation. If Route 53 has already returned a page or more of results, if @IsTruncated@ is true, and if @NextContinentCode@ from the previous response has a value, enter that value in @startcontinentcode@ to return the next page of results.
    --
    -- Include @startcontinentcode@ only if you want to list continents. Don't include @startcontinentcode@ when you're listing countries or countries with their subdivisions.
    startContinentCode :: Core.Maybe Types.StartContinentCode,
    -- | The code for the country with which you want to start listing locations that Amazon Route 53 supports for geolocation. If Route 53 has already returned a page or more of results, if @IsTruncated@ is @true@ , and if @NextCountryCode@ from the previous response has a value, enter that value in @startcountrycode@ to return the next page of results.
    startCountryCode :: Core.Maybe Types.GeoLocationCountryCode,
    -- | The code for the state of the United States with which you want to start listing locations that Amazon Route 53 supports for geolocation. If Route 53 has already returned a page or more of results, if @IsTruncated@ is @true@ , and if @NextSubdivisionCode@ from the previous response has a value, enter that value in @startsubdivisioncode@ to return the next page of results.
    --
    -- To list subdivisions (U.S. states), you must include both @startcountrycode@ and @startsubdivisioncode@ .
    startSubdivisionCode :: Core.Maybe Types.StartSubdivisionCode
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListGeoLocations' value with any optional fields omitted.
mkListGeoLocations ::
  ListGeoLocations
mkListGeoLocations =
  ListGeoLocations'
    { maxItems = Core.Nothing,
      startContinentCode = Core.Nothing,
      startCountryCode = Core.Nothing,
      startSubdivisionCode = Core.Nothing
    }

-- | (Optional) The maximum number of geolocations to be included in the response body for this request. If more than @maxitems@ geolocations remain to be listed, then the value of the @IsTruncated@ element in the response is @true@ .
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lglMaxItems :: Lens.Lens' ListGeoLocations (Core.Maybe Types.MaxItems)
lglMaxItems = Lens.field @"maxItems"
{-# DEPRECATED lglMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | The code for the continent with which you want to start listing locations that Amazon Route 53 supports for geolocation. If Route 53 has already returned a page or more of results, if @IsTruncated@ is true, and if @NextContinentCode@ from the previous response has a value, enter that value in @startcontinentcode@ to return the next page of results.
--
-- Include @startcontinentcode@ only if you want to list continents. Don't include @startcontinentcode@ when you're listing countries or countries with their subdivisions.
--
-- /Note:/ Consider using 'startContinentCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lglStartContinentCode :: Lens.Lens' ListGeoLocations (Core.Maybe Types.StartContinentCode)
lglStartContinentCode = Lens.field @"startContinentCode"
{-# DEPRECATED lglStartContinentCode "Use generic-lens or generic-optics with 'startContinentCode' instead." #-}

-- | The code for the country with which you want to start listing locations that Amazon Route 53 supports for geolocation. If Route 53 has already returned a page or more of results, if @IsTruncated@ is @true@ , and if @NextCountryCode@ from the previous response has a value, enter that value in @startcountrycode@ to return the next page of results.
--
-- /Note:/ Consider using 'startCountryCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lglStartCountryCode :: Lens.Lens' ListGeoLocations (Core.Maybe Types.GeoLocationCountryCode)
lglStartCountryCode = Lens.field @"startCountryCode"
{-# DEPRECATED lglStartCountryCode "Use generic-lens or generic-optics with 'startCountryCode' instead." #-}

-- | The code for the state of the United States with which you want to start listing locations that Amazon Route 53 supports for geolocation. If Route 53 has already returned a page or more of results, if @IsTruncated@ is @true@ , and if @NextSubdivisionCode@ from the previous response has a value, enter that value in @startsubdivisioncode@ to return the next page of results.
--
-- To list subdivisions (U.S. states), you must include both @startcountrycode@ and @startsubdivisioncode@ .
--
-- /Note:/ Consider using 'startSubdivisionCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lglStartSubdivisionCode :: Lens.Lens' ListGeoLocations (Core.Maybe Types.StartSubdivisionCode)
lglStartSubdivisionCode = Lens.field @"startSubdivisionCode"
{-# DEPRECATED lglStartSubdivisionCode "Use generic-lens or generic-optics with 'startSubdivisionCode' instead." #-}

instance Core.AWSRequest ListGeoLocations where
  type Rs ListGeoLocations = ListGeoLocationsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/2013-04-01/geolocations",
        Core._rqQuery =
          Core.toQueryValue "maxitems" Core.<$> maxItems
            Core.<> ( Core.toQueryValue "startcontinentcode"
                        Core.<$> startContinentCode
                    )
            Core.<> (Core.toQueryValue "startcountrycode" Core.<$> startCountryCode)
            Core.<> ( Core.toQueryValue "startsubdivisioncode"
                        Core.<$> startSubdivisionCode
                    ),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveXML
      ( \s h x ->
          ListGeoLocationsResponse'
            Core.<$> ( x Core..@? "GeoLocationDetailsList" Core..@! Core.mempty
                         Core..<@> Core.parseXMLList "GeoLocationDetails"
                     )
            Core.<*> (x Core..@ "IsTruncated")
            Core.<*> (x Core..@ "MaxItems")
            Core.<*> (x Core..@? "NextContinentCode")
            Core.<*> (x Core..@? "NextCountryCode")
            Core.<*> (x Core..@? "NextSubdivisionCode")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | A complex type containing the response information for the request.
--
-- /See:/ 'mkListGeoLocationsResponse' smart constructor.
data ListGeoLocationsResponse = ListGeoLocationsResponse'
  { -- | A complex type that contains one @GeoLocationDetails@ element for each location that Amazon Route 53 supports for geolocation.
    geoLocationDetailsList :: [Types.GeoLocationDetails],
    -- | A value that indicates whether more locations remain to be listed after the last location in this response. If so, the value of @IsTruncated@ is @true@ . To get more values, submit another request and include the values of @NextContinentCode@ , @NextCountryCode@ , and @NextSubdivisionCode@ in the @startcontinentcode@ , @startcountrycode@ , and @startsubdivisioncode@ , as applicable.
    isTruncated :: Core.Bool,
    -- | The value that you specified for @MaxItems@ in the request.
    maxItems :: Types.MaxItems,
    -- | If @IsTruncated@ is @true@ , you can make a follow-up request to display more locations. Enter the value of @NextContinentCode@ in the @startcontinentcode@ parameter in another @ListGeoLocations@ request.
    nextContinentCode :: Core.Maybe Types.NextContinentCode,
    -- | If @IsTruncated@ is @true@ , you can make a follow-up request to display more locations. Enter the value of @NextCountryCode@ in the @startcountrycode@ parameter in another @ListGeoLocations@ request.
    nextCountryCode :: Core.Maybe Types.NextCountryCode,
    -- | If @IsTruncated@ is @true@ , you can make a follow-up request to display more locations. Enter the value of @NextSubdivisionCode@ in the @startsubdivisioncode@ parameter in another @ListGeoLocations@ request.
    nextSubdivisionCode :: Core.Maybe Types.NextSubdivisionCode,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListGeoLocationsResponse' value with any optional fields omitted.
mkListGeoLocationsResponse ::
  -- | 'isTruncated'
  Core.Bool ->
  -- | 'maxItems'
  Types.MaxItems ->
  -- | 'responseStatus'
  Core.Int ->
  ListGeoLocationsResponse
mkListGeoLocationsResponse isTruncated maxItems responseStatus =
  ListGeoLocationsResponse'
    { geoLocationDetailsList = Core.mempty,
      isTruncated,
      maxItems,
      nextContinentCode = Core.Nothing,
      nextCountryCode = Core.Nothing,
      nextSubdivisionCode = Core.Nothing,
      responseStatus
    }

-- | A complex type that contains one @GeoLocationDetails@ element for each location that Amazon Route 53 supports for geolocation.
--
-- /Note:/ Consider using 'geoLocationDetailsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lglrrsGeoLocationDetailsList :: Lens.Lens' ListGeoLocationsResponse [Types.GeoLocationDetails]
lglrrsGeoLocationDetailsList = Lens.field @"geoLocationDetailsList"
{-# DEPRECATED lglrrsGeoLocationDetailsList "Use generic-lens or generic-optics with 'geoLocationDetailsList' instead." #-}

-- | A value that indicates whether more locations remain to be listed after the last location in this response. If so, the value of @IsTruncated@ is @true@ . To get more values, submit another request and include the values of @NextContinentCode@ , @NextCountryCode@ , and @NextSubdivisionCode@ in the @startcontinentcode@ , @startcountrycode@ , and @startsubdivisioncode@ , as applicable.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lglrrsIsTruncated :: Lens.Lens' ListGeoLocationsResponse Core.Bool
lglrrsIsTruncated = Lens.field @"isTruncated"
{-# DEPRECATED lglrrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The value that you specified for @MaxItems@ in the request.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lglrrsMaxItems :: Lens.Lens' ListGeoLocationsResponse Types.MaxItems
lglrrsMaxItems = Lens.field @"maxItems"
{-# DEPRECATED lglrrsMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | If @IsTruncated@ is @true@ , you can make a follow-up request to display more locations. Enter the value of @NextContinentCode@ in the @startcontinentcode@ parameter in another @ListGeoLocations@ request.
--
-- /Note:/ Consider using 'nextContinentCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lglrrsNextContinentCode :: Lens.Lens' ListGeoLocationsResponse (Core.Maybe Types.NextContinentCode)
lglrrsNextContinentCode = Lens.field @"nextContinentCode"
{-# DEPRECATED lglrrsNextContinentCode "Use generic-lens or generic-optics with 'nextContinentCode' instead." #-}

-- | If @IsTruncated@ is @true@ , you can make a follow-up request to display more locations. Enter the value of @NextCountryCode@ in the @startcountrycode@ parameter in another @ListGeoLocations@ request.
--
-- /Note:/ Consider using 'nextCountryCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lglrrsNextCountryCode :: Lens.Lens' ListGeoLocationsResponse (Core.Maybe Types.NextCountryCode)
lglrrsNextCountryCode = Lens.field @"nextCountryCode"
{-# DEPRECATED lglrrsNextCountryCode "Use generic-lens or generic-optics with 'nextCountryCode' instead." #-}

-- | If @IsTruncated@ is @true@ , you can make a follow-up request to display more locations. Enter the value of @NextSubdivisionCode@ in the @startsubdivisioncode@ parameter in another @ListGeoLocations@ request.
--
-- /Note:/ Consider using 'nextSubdivisionCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lglrrsNextSubdivisionCode :: Lens.Lens' ListGeoLocationsResponse (Core.Maybe Types.NextSubdivisionCode)
lglrrsNextSubdivisionCode = Lens.field @"nextSubdivisionCode"
{-# DEPRECATED lglrrsNextSubdivisionCode "Use generic-lens or generic-optics with 'nextSubdivisionCode' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lglrrsResponseStatus :: Lens.Lens' ListGeoLocationsResponse Core.Int
lglrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lglrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
