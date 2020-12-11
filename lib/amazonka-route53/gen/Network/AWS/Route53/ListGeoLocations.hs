{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    lglStartSubdivisionCode,
    lglMaxItems,
    lglStartCountryCode,
    lglStartContinentCode,

    -- * Destructuring the response
    ListGeoLocationsResponse (..),
    mkListGeoLocationsResponse,

    -- ** Response lenses
    lglrsNextContinentCode,
    lglrsNextCountryCode,
    lglrsNextSubdivisionCode,
    lglrsResponseStatus,
    lglrsGeoLocationDetailsList,
    lglrsIsTruncated,
    lglrsMaxItems,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Route53.Types

-- | A request to get a list of geographic locations that Amazon Route 53 supports for geolocation resource record sets.
--
-- /See:/ 'mkListGeoLocations' smart constructor.
data ListGeoLocations = ListGeoLocations'
  { startSubdivisionCode ::
      Lude.Maybe Lude.Text,
    maxItems :: Lude.Maybe Lude.Text,
    startCountryCode :: Lude.Maybe Lude.Text,
    startContinentCode :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListGeoLocations' with the minimum fields required to make a request.
--
-- * 'maxItems' - (Optional) The maximum number of geolocations to be included in the response body for this request. If more than @maxitems@ geolocations remain to be listed, then the value of the @IsTruncated@ element in the response is @true@ .
-- * 'startContinentCode' - The code for the continent with which you want to start listing locations that Amazon Route 53 supports for geolocation. If Route 53 has already returned a page or more of results, if @IsTruncated@ is true, and if @NextContinentCode@ from the previous response has a value, enter that value in @startcontinentcode@ to return the next page of results.
--
-- Include @startcontinentcode@ only if you want to list continents. Don't include @startcontinentcode@ when you're listing countries or countries with their subdivisions.
-- * 'startCountryCode' - The code for the country with which you want to start listing locations that Amazon Route 53 supports for geolocation. If Route 53 has already returned a page or more of results, if @IsTruncated@ is @true@ , and if @NextCountryCode@ from the previous response has a value, enter that value in @startcountrycode@ to return the next page of results.
-- * 'startSubdivisionCode' - The code for the state of the United States with which you want to start listing locations that Amazon Route 53 supports for geolocation. If Route 53 has already returned a page or more of results, if @IsTruncated@ is @true@ , and if @NextSubdivisionCode@ from the previous response has a value, enter that value in @startsubdivisioncode@ to return the next page of results.
--
-- To list subdivisions (U.S. states), you must include both @startcountrycode@ and @startsubdivisioncode@ .
mkListGeoLocations ::
  ListGeoLocations
mkListGeoLocations =
  ListGeoLocations'
    { startSubdivisionCode = Lude.Nothing,
      maxItems = Lude.Nothing,
      startCountryCode = Lude.Nothing,
      startContinentCode = Lude.Nothing
    }

-- | The code for the state of the United States with which you want to start listing locations that Amazon Route 53 supports for geolocation. If Route 53 has already returned a page or more of results, if @IsTruncated@ is @true@ , and if @NextSubdivisionCode@ from the previous response has a value, enter that value in @startsubdivisioncode@ to return the next page of results.
--
-- To list subdivisions (U.S. states), you must include both @startcountrycode@ and @startsubdivisioncode@ .
--
-- /Note:/ Consider using 'startSubdivisionCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lglStartSubdivisionCode :: Lens.Lens' ListGeoLocations (Lude.Maybe Lude.Text)
lglStartSubdivisionCode = Lens.lens (startSubdivisionCode :: ListGeoLocations -> Lude.Maybe Lude.Text) (\s a -> s {startSubdivisionCode = a} :: ListGeoLocations)
{-# DEPRECATED lglStartSubdivisionCode "Use generic-lens or generic-optics with 'startSubdivisionCode' instead." #-}

-- | (Optional) The maximum number of geolocations to be included in the response body for this request. If more than @maxitems@ geolocations remain to be listed, then the value of the @IsTruncated@ element in the response is @true@ .
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lglMaxItems :: Lens.Lens' ListGeoLocations (Lude.Maybe Lude.Text)
lglMaxItems = Lens.lens (maxItems :: ListGeoLocations -> Lude.Maybe Lude.Text) (\s a -> s {maxItems = a} :: ListGeoLocations)
{-# DEPRECATED lglMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

-- | The code for the country with which you want to start listing locations that Amazon Route 53 supports for geolocation. If Route 53 has already returned a page or more of results, if @IsTruncated@ is @true@ , and if @NextCountryCode@ from the previous response has a value, enter that value in @startcountrycode@ to return the next page of results.
--
-- /Note:/ Consider using 'startCountryCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lglStartCountryCode :: Lens.Lens' ListGeoLocations (Lude.Maybe Lude.Text)
lglStartCountryCode = Lens.lens (startCountryCode :: ListGeoLocations -> Lude.Maybe Lude.Text) (\s a -> s {startCountryCode = a} :: ListGeoLocations)
{-# DEPRECATED lglStartCountryCode "Use generic-lens or generic-optics with 'startCountryCode' instead." #-}

-- | The code for the continent with which you want to start listing locations that Amazon Route 53 supports for geolocation. If Route 53 has already returned a page or more of results, if @IsTruncated@ is true, and if @NextContinentCode@ from the previous response has a value, enter that value in @startcontinentcode@ to return the next page of results.
--
-- Include @startcontinentcode@ only if you want to list continents. Don't include @startcontinentcode@ when you're listing countries or countries with their subdivisions.
--
-- /Note:/ Consider using 'startContinentCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lglStartContinentCode :: Lens.Lens' ListGeoLocations (Lude.Maybe Lude.Text)
lglStartContinentCode = Lens.lens (startContinentCode :: ListGeoLocations -> Lude.Maybe Lude.Text) (\s a -> s {startContinentCode = a} :: ListGeoLocations)
{-# DEPRECATED lglStartContinentCode "Use generic-lens or generic-optics with 'startContinentCode' instead." #-}

instance Lude.AWSRequest ListGeoLocations where
  type Rs ListGeoLocations = ListGeoLocationsResponse
  request = Req.get route53Service
  response =
    Res.receiveXML
      ( \s h x ->
          ListGeoLocationsResponse'
            Lude.<$> (x Lude..@? "NextContinentCode")
            Lude.<*> (x Lude..@? "NextCountryCode")
            Lude.<*> (x Lude..@? "NextSubdivisionCode")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> ( x Lude..@? "GeoLocationDetailsList" Lude..!@ Lude.mempty
                         Lude.>>= Lude.parseXMLList "GeoLocationDetails"
                     )
            Lude.<*> (x Lude..@ "IsTruncated")
            Lude.<*> (x Lude..@ "MaxItems")
      )

instance Lude.ToHeaders ListGeoLocations where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListGeoLocations where
  toPath = Lude.const "/2013-04-01/geolocations"

instance Lude.ToQuery ListGeoLocations where
  toQuery ListGeoLocations' {..} =
    Lude.mconcat
      [ "startsubdivisioncode" Lude.=: startSubdivisionCode,
        "maxitems" Lude.=: maxItems,
        "startcountrycode" Lude.=: startCountryCode,
        "startcontinentcode" Lude.=: startContinentCode
      ]

-- | A complex type containing the response information for the request.
--
-- /See:/ 'mkListGeoLocationsResponse' smart constructor.
data ListGeoLocationsResponse = ListGeoLocationsResponse'
  { nextContinentCode ::
      Lude.Maybe Lude.Text,
    nextCountryCode :: Lude.Maybe Lude.Text,
    nextSubdivisionCode ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    geoLocationDetailsList ::
      [GeoLocationDetails],
    isTruncated :: Lude.Bool,
    maxItems :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListGeoLocationsResponse' with the minimum fields required to make a request.
--
-- * 'geoLocationDetailsList' - A complex type that contains one @GeoLocationDetails@ element for each location that Amazon Route 53 supports for geolocation.
-- * 'isTruncated' - A value that indicates whether more locations remain to be listed after the last location in this response. If so, the value of @IsTruncated@ is @true@ . To get more values, submit another request and include the values of @NextContinentCode@ , @NextCountryCode@ , and @NextSubdivisionCode@ in the @startcontinentcode@ , @startcountrycode@ , and @startsubdivisioncode@ , as applicable.
-- * 'maxItems' - The value that you specified for @MaxItems@ in the request.
-- * 'nextContinentCode' - If @IsTruncated@ is @true@ , you can make a follow-up request to display more locations. Enter the value of @NextContinentCode@ in the @startcontinentcode@ parameter in another @ListGeoLocations@ request.
-- * 'nextCountryCode' - If @IsTruncated@ is @true@ , you can make a follow-up request to display more locations. Enter the value of @NextCountryCode@ in the @startcountrycode@ parameter in another @ListGeoLocations@ request.
-- * 'nextSubdivisionCode' - If @IsTruncated@ is @true@ , you can make a follow-up request to display more locations. Enter the value of @NextSubdivisionCode@ in the @startsubdivisioncode@ parameter in another @ListGeoLocations@ request.
-- * 'responseStatus' - The response status code.
mkListGeoLocationsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'isTruncated'
  Lude.Bool ->
  -- | 'maxItems'
  Lude.Text ->
  ListGeoLocationsResponse
mkListGeoLocationsResponse
  pResponseStatus_
  pIsTruncated_
  pMaxItems_ =
    ListGeoLocationsResponse'
      { nextContinentCode = Lude.Nothing,
        nextCountryCode = Lude.Nothing,
        nextSubdivisionCode = Lude.Nothing,
        responseStatus = pResponseStatus_,
        geoLocationDetailsList = Lude.mempty,
        isTruncated = pIsTruncated_,
        maxItems = pMaxItems_
      }

-- | If @IsTruncated@ is @true@ , you can make a follow-up request to display more locations. Enter the value of @NextContinentCode@ in the @startcontinentcode@ parameter in another @ListGeoLocations@ request.
--
-- /Note:/ Consider using 'nextContinentCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lglrsNextContinentCode :: Lens.Lens' ListGeoLocationsResponse (Lude.Maybe Lude.Text)
lglrsNextContinentCode = Lens.lens (nextContinentCode :: ListGeoLocationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextContinentCode = a} :: ListGeoLocationsResponse)
{-# DEPRECATED lglrsNextContinentCode "Use generic-lens or generic-optics with 'nextContinentCode' instead." #-}

-- | If @IsTruncated@ is @true@ , you can make a follow-up request to display more locations. Enter the value of @NextCountryCode@ in the @startcountrycode@ parameter in another @ListGeoLocations@ request.
--
-- /Note:/ Consider using 'nextCountryCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lglrsNextCountryCode :: Lens.Lens' ListGeoLocationsResponse (Lude.Maybe Lude.Text)
lglrsNextCountryCode = Lens.lens (nextCountryCode :: ListGeoLocationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextCountryCode = a} :: ListGeoLocationsResponse)
{-# DEPRECATED lglrsNextCountryCode "Use generic-lens or generic-optics with 'nextCountryCode' instead." #-}

-- | If @IsTruncated@ is @true@ , you can make a follow-up request to display more locations. Enter the value of @NextSubdivisionCode@ in the @startsubdivisioncode@ parameter in another @ListGeoLocations@ request.
--
-- /Note:/ Consider using 'nextSubdivisionCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lglrsNextSubdivisionCode :: Lens.Lens' ListGeoLocationsResponse (Lude.Maybe Lude.Text)
lglrsNextSubdivisionCode = Lens.lens (nextSubdivisionCode :: ListGeoLocationsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextSubdivisionCode = a} :: ListGeoLocationsResponse)
{-# DEPRECATED lglrsNextSubdivisionCode "Use generic-lens or generic-optics with 'nextSubdivisionCode' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lglrsResponseStatus :: Lens.Lens' ListGeoLocationsResponse Lude.Int
lglrsResponseStatus = Lens.lens (responseStatus :: ListGeoLocationsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListGeoLocationsResponse)
{-# DEPRECATED lglrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A complex type that contains one @GeoLocationDetails@ element for each location that Amazon Route 53 supports for geolocation.
--
-- /Note:/ Consider using 'geoLocationDetailsList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lglrsGeoLocationDetailsList :: Lens.Lens' ListGeoLocationsResponse [GeoLocationDetails]
lglrsGeoLocationDetailsList = Lens.lens (geoLocationDetailsList :: ListGeoLocationsResponse -> [GeoLocationDetails]) (\s a -> s {geoLocationDetailsList = a} :: ListGeoLocationsResponse)
{-# DEPRECATED lglrsGeoLocationDetailsList "Use generic-lens or generic-optics with 'geoLocationDetailsList' instead." #-}

-- | A value that indicates whether more locations remain to be listed after the last location in this response. If so, the value of @IsTruncated@ is @true@ . To get more values, submit another request and include the values of @NextContinentCode@ , @NextCountryCode@ , and @NextSubdivisionCode@ in the @startcontinentcode@ , @startcountrycode@ , and @startsubdivisioncode@ , as applicable.
--
-- /Note:/ Consider using 'isTruncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lglrsIsTruncated :: Lens.Lens' ListGeoLocationsResponse Lude.Bool
lglrsIsTruncated = Lens.lens (isTruncated :: ListGeoLocationsResponse -> Lude.Bool) (\s a -> s {isTruncated = a} :: ListGeoLocationsResponse)
{-# DEPRECATED lglrsIsTruncated "Use generic-lens or generic-optics with 'isTruncated' instead." #-}

-- | The value that you specified for @MaxItems@ in the request.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lglrsMaxItems :: Lens.Lens' ListGeoLocationsResponse Lude.Text
lglrsMaxItems = Lens.lens (maxItems :: ListGeoLocationsResponse -> Lude.Text) (\s a -> s {maxItems = a} :: ListGeoLocationsResponse)
{-# DEPRECATED lglrsMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}
