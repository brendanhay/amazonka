{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.ListGeoLocations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of supported geo locations.
--
--
-- Countries are listed first, and continents are listed last. If Amazon Route 53 supports subdivisions for a country (for example, states or provinces), the subdivisions for that country are listed in alphabetical order immediately after the corresponding country.
--
module Network.AWS.Route53.ListGeoLocations
    (
    -- * Creating a Request
      listGeoLocations
    , ListGeoLocations
    -- * Request Lenses
    , lglStartSubdivisionCode
    , lglMaxItems
    , lglStartCountryCode
    , lglStartContinentCode

    -- * Destructuring the Response
    , listGeoLocationsResponse
    , ListGeoLocationsResponse
    -- * Response Lenses
    , lglrsNextContinentCode
    , lglrsNextCountryCode
    , lglrsNextSubdivisionCode
    , lglrsResponseStatus
    , lglrsGeoLocationDetailsList
    , lglrsIsTruncated
    , lglrsMaxItems
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Route53.Types
import Network.AWS.Route53.Types.Product

-- | A request to get a list of geographic locations that Amazon Route 53 supports for geolocation resource record sets.
--
--
--
-- /See:/ 'listGeoLocations' smart constructor.
data ListGeoLocations = ListGeoLocations'
  { _lglStartSubdivisionCode :: !(Maybe Text)
  , _lglMaxItems             :: !(Maybe Text)
  , _lglStartCountryCode     :: !(Maybe Text)
  , _lglStartContinentCode   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListGeoLocations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lglStartSubdivisionCode' - The code for the subdivision (for example, state or province) with which you want to start listing locations that Amazon Route 53 supports for geolocation. If Amazon Route 53 has already returned a page or more of results, if @IsTruncated@ is @true@ , and if @NextSubdivisionCode@ from the previous response has a value, enter that value in @StartSubdivisionCode@ to return the next page of results. To list subdivisions of a country, you must include both @StartCountryCode@ and @StartSubdivisionCode@ .
--
-- * 'lglMaxItems' - (Optional) The maximum number of geolocations to be included in the response body for this request. If more than @MaxItems@ geolocations remain to be listed, then the value of the @IsTruncated@ element in the response is @true@ .
--
-- * 'lglStartCountryCode' - The code for the country with which you want to start listing locations that Amazon Route 53 supports for geolocation. If Amazon Route 53 has already returned a page or more of results, if @IsTruncated@ is @true@ , and if @NextCountryCode@ from the previous response has a value, enter that value in @StartCountryCode@ to return the next page of results. Amazon Route 53 uses the two-letter country codes that are specified in <https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2 ISO standard 3166-1 alpha-2> .
--
-- * 'lglStartContinentCode' - The code for the continent with which you want to start listing locations that Amazon Route 53 supports for geolocation. If Amazon Route 53 has already returned a page or more of results, if @IsTruncated@ is true, and if @NextContinentCode@ from the previous response has a value, enter that value in @StartContinentCode@ to return the next page of results. Include @StartContinentCode@ only if you want to list continents. Don't include @StartContinentCode@ when you're listing countries or countries with their subdivisions.
listGeoLocations
    :: ListGeoLocations
listGeoLocations =
  ListGeoLocations'
    { _lglStartSubdivisionCode = Nothing
    , _lglMaxItems = Nothing
    , _lglStartCountryCode = Nothing
    , _lglStartContinentCode = Nothing
    }


-- | The code for the subdivision (for example, state or province) with which you want to start listing locations that Amazon Route 53 supports for geolocation. If Amazon Route 53 has already returned a page or more of results, if @IsTruncated@ is @true@ , and if @NextSubdivisionCode@ from the previous response has a value, enter that value in @StartSubdivisionCode@ to return the next page of results. To list subdivisions of a country, you must include both @StartCountryCode@ and @StartSubdivisionCode@ .
lglStartSubdivisionCode :: Lens' ListGeoLocations (Maybe Text)
lglStartSubdivisionCode = lens _lglStartSubdivisionCode (\ s a -> s{_lglStartSubdivisionCode = a})

-- | (Optional) The maximum number of geolocations to be included in the response body for this request. If more than @MaxItems@ geolocations remain to be listed, then the value of the @IsTruncated@ element in the response is @true@ .
lglMaxItems :: Lens' ListGeoLocations (Maybe Text)
lglMaxItems = lens _lglMaxItems (\ s a -> s{_lglMaxItems = a})

-- | The code for the country with which you want to start listing locations that Amazon Route 53 supports for geolocation. If Amazon Route 53 has already returned a page or more of results, if @IsTruncated@ is @true@ , and if @NextCountryCode@ from the previous response has a value, enter that value in @StartCountryCode@ to return the next page of results. Amazon Route 53 uses the two-letter country codes that are specified in <https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2 ISO standard 3166-1 alpha-2> .
lglStartCountryCode :: Lens' ListGeoLocations (Maybe Text)
lglStartCountryCode = lens _lglStartCountryCode (\ s a -> s{_lglStartCountryCode = a})

-- | The code for the continent with which you want to start listing locations that Amazon Route 53 supports for geolocation. If Amazon Route 53 has already returned a page or more of results, if @IsTruncated@ is true, and if @NextContinentCode@ from the previous response has a value, enter that value in @StartContinentCode@ to return the next page of results. Include @StartContinentCode@ only if you want to list continents. Don't include @StartContinentCode@ when you're listing countries or countries with their subdivisions.
lglStartContinentCode :: Lens' ListGeoLocations (Maybe Text)
lglStartContinentCode = lens _lglStartContinentCode (\ s a -> s{_lglStartContinentCode = a})

instance AWSRequest ListGeoLocations where
        type Rs ListGeoLocations = ListGeoLocationsResponse
        request = get route53
        response
          = receiveXML
              (\ s h x ->
                 ListGeoLocationsResponse' <$>
                   (x .@? "NextContinentCode") <*>
                     (x .@? "NextCountryCode")
                     <*> (x .@? "NextSubdivisionCode")
                     <*> (pure (fromEnum s))
                     <*>
                     (x .@? "GeoLocationDetailsList" .!@ mempty >>=
                        parseXMLList "GeoLocationDetails")
                     <*> (x .@ "IsTruncated")
                     <*> (x .@ "MaxItems"))

instance Hashable ListGeoLocations where

instance NFData ListGeoLocations where

instance ToHeaders ListGeoLocations where
        toHeaders = const mempty

instance ToPath ListGeoLocations where
        toPath = const "/2013-04-01/geolocations"

instance ToQuery ListGeoLocations where
        toQuery ListGeoLocations'{..}
          = mconcat
              ["startsubdivisioncode" =: _lglStartSubdivisionCode,
               "maxitems" =: _lglMaxItems,
               "startcountrycode" =: _lglStartCountryCode,
               "startcontinentcode" =: _lglStartContinentCode]

-- | A complex type containing the response information for the request.
--
--
--
-- /See:/ 'listGeoLocationsResponse' smart constructor.
data ListGeoLocationsResponse = ListGeoLocationsResponse'
  { _lglrsNextContinentCode      :: !(Maybe Text)
  , _lglrsNextCountryCode        :: !(Maybe Text)
  , _lglrsNextSubdivisionCode    :: !(Maybe Text)
  , _lglrsResponseStatus         :: !Int
  , _lglrsGeoLocationDetailsList :: ![GeoLocationDetails]
  , _lglrsIsTruncated            :: !Bool
  , _lglrsMaxItems               :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListGeoLocationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lglrsNextContinentCode' - If @IsTruncated@ is @true@ , you can make a follow-up request to display more locations. Enter the value of @NextContinentCode@ in the @StartContinentCode@ parameter in another @ListGeoLocations@ request.
--
-- * 'lglrsNextCountryCode' - If @IsTruncated@ is @true@ , you can make a follow-up request to display more locations. Enter the value of @NextCountryCode@ in the @StartCountryCode@ parameter in another @ListGeoLocations@ request.
--
-- * 'lglrsNextSubdivisionCode' - If @IsTruncated@ is @true@ , you can make a follow-up request to display more locations. Enter the value of @NextSubdivisionCode@ in the @StartSubdivisionCode@ parameter in another @ListGeoLocations@ request.
--
-- * 'lglrsResponseStatus' - -- | The response status code.
--
-- * 'lglrsGeoLocationDetailsList' - A complex type that contains one @GeoLocationDetails@ element for each location that Amazon Route 53 supports for geolocation.
--
-- * 'lglrsIsTruncated' - A value that indicates whether more locations remain to be listed after the last location in this response. If so, the value of @IsTruncated@ is @true@ . To get more values, submit another request and include the values of @NextContinentCode@ , @NextCountryCode@ , and @NextSubdivisionCode@ in the @StartContinentCode@ , @StartCountryCode@ , and @StartSubdivisionCode@ , as applicable.
--
-- * 'lglrsMaxItems' - The value that you specified for @MaxItems@ in the request.
listGeoLocationsResponse
    :: Int -- ^ 'lglrsResponseStatus'
    -> Bool -- ^ 'lglrsIsTruncated'
    -> Text -- ^ 'lglrsMaxItems'
    -> ListGeoLocationsResponse
listGeoLocationsResponse pResponseStatus_ pIsTruncated_ pMaxItems_ =
  ListGeoLocationsResponse'
    { _lglrsNextContinentCode = Nothing
    , _lglrsNextCountryCode = Nothing
    , _lglrsNextSubdivisionCode = Nothing
    , _lglrsResponseStatus = pResponseStatus_
    , _lglrsGeoLocationDetailsList = mempty
    , _lglrsIsTruncated = pIsTruncated_
    , _lglrsMaxItems = pMaxItems_
    }


-- | If @IsTruncated@ is @true@ , you can make a follow-up request to display more locations. Enter the value of @NextContinentCode@ in the @StartContinentCode@ parameter in another @ListGeoLocations@ request.
lglrsNextContinentCode :: Lens' ListGeoLocationsResponse (Maybe Text)
lglrsNextContinentCode = lens _lglrsNextContinentCode (\ s a -> s{_lglrsNextContinentCode = a})

-- | If @IsTruncated@ is @true@ , you can make a follow-up request to display more locations. Enter the value of @NextCountryCode@ in the @StartCountryCode@ parameter in another @ListGeoLocations@ request.
lglrsNextCountryCode :: Lens' ListGeoLocationsResponse (Maybe Text)
lglrsNextCountryCode = lens _lglrsNextCountryCode (\ s a -> s{_lglrsNextCountryCode = a})

-- | If @IsTruncated@ is @true@ , you can make a follow-up request to display more locations. Enter the value of @NextSubdivisionCode@ in the @StartSubdivisionCode@ parameter in another @ListGeoLocations@ request.
lglrsNextSubdivisionCode :: Lens' ListGeoLocationsResponse (Maybe Text)
lglrsNextSubdivisionCode = lens _lglrsNextSubdivisionCode (\ s a -> s{_lglrsNextSubdivisionCode = a})

-- | -- | The response status code.
lglrsResponseStatus :: Lens' ListGeoLocationsResponse Int
lglrsResponseStatus = lens _lglrsResponseStatus (\ s a -> s{_lglrsResponseStatus = a})

-- | A complex type that contains one @GeoLocationDetails@ element for each location that Amazon Route 53 supports for geolocation.
lglrsGeoLocationDetailsList :: Lens' ListGeoLocationsResponse [GeoLocationDetails]
lglrsGeoLocationDetailsList = lens _lglrsGeoLocationDetailsList (\ s a -> s{_lglrsGeoLocationDetailsList = a}) . _Coerce

-- | A value that indicates whether more locations remain to be listed after the last location in this response. If so, the value of @IsTruncated@ is @true@ . To get more values, submit another request and include the values of @NextContinentCode@ , @NextCountryCode@ , and @NextSubdivisionCode@ in the @StartContinentCode@ , @StartCountryCode@ , and @StartSubdivisionCode@ , as applicable.
lglrsIsTruncated :: Lens' ListGeoLocationsResponse Bool
lglrsIsTruncated = lens _lglrsIsTruncated (\ s a -> s{_lglrsIsTruncated = a})

-- | The value that you specified for @MaxItems@ in the request.
lglrsMaxItems :: Lens' ListGeoLocationsResponse Text
lglrsMaxItems = lens _lglrsMaxItems (\ s a -> s{_lglrsMaxItems = a})

instance NFData ListGeoLocationsResponse where
