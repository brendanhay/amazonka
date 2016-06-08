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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- To retrieve a list of supported geo locations, send a 'GET' request to the '\/Route 53 API version\/geolocations' resource. The response to this request includes a 'GeoLocationDetailsList' element with zero, one, or multiple 'GeoLocationDetails' child elements. The list is sorted by country code, and then subdivision code, followed by continents at the end of the list.
--
-- By default, the list of geo locations is displayed on a single page. You can control the length of the page that is displayed by using the 'MaxItems' parameter. If the list is truncated, 'IsTruncated' will be set to /true/ and a combination of 'NextContinentCode, NextCountryCode, NextSubdivisionCode' will be populated. You can pass these as parameters to 'StartContinentCode, StartCountryCode, StartSubdivisionCode' to control the geo location that the list begins with.
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

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types
import           Network.AWS.Route53.Types.Product

-- | The input for a 'ListGeoLocations' request.
--
-- /See:/ 'listGeoLocations' smart constructor.
data ListGeoLocations = ListGeoLocations'
    { _lglStartSubdivisionCode :: !(Maybe Text)
    , _lglMaxItems             :: !(Maybe Text)
    , _lglStartCountryCode     :: !(Maybe Text)
    , _lglStartContinentCode   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListGeoLocations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lglStartSubdivisionCode'
--
-- * 'lglMaxItems'
--
-- * 'lglStartCountryCode'
--
-- * 'lglStartContinentCode'
listGeoLocations
    :: ListGeoLocations
listGeoLocations =
    ListGeoLocations'
    { _lglStartSubdivisionCode = Nothing
    , _lglMaxItems = Nothing
    , _lglStartCountryCode = Nothing
    , _lglStartContinentCode = Nothing
    }

-- | The first subdivision code in the lexicographic ordering of geo locations that you want the 'ListGeoLocations' request to list.
--
-- Constraint: Specifying 'SubdivisionCode' without 'CountryCode' returns an < InvalidInput> error.
lglStartSubdivisionCode :: Lens' ListGeoLocations (Maybe Text)
lglStartSubdivisionCode = lens _lglStartSubdivisionCode (\ s a -> s{_lglStartSubdivisionCode = a});

-- | The maximum number of geo locations you want in the response body.
lglMaxItems :: Lens' ListGeoLocations (Maybe Text)
lglMaxItems = lens _lglMaxItems (\ s a -> s{_lglMaxItems = a});

-- | The first country code in the lexicographic ordering of geo locations that you want the 'ListGeoLocations' request to list.
--
-- The default geo location uses a '*' for the country code. All other country codes follow the ISO 3166 two-character code.
lglStartCountryCode :: Lens' ListGeoLocations (Maybe Text)
lglStartCountryCode = lens _lglStartCountryCode (\ s a -> s{_lglStartCountryCode = a});

-- | The first continent code in the lexicographic ordering of geo locations that you want the 'ListGeoLocations' request to list. For non-continent geo locations, this should be null.
--
-- Valid values: 'AF' | 'AN' | 'AS' | 'EU' | 'OC' | 'NA' | 'SA'
--
-- Constraint: Specifying 'ContinentCode' with either 'CountryCode' or 'SubdivisionCode' returns an < InvalidInput> error.
lglStartContinentCode :: Lens' ListGeoLocations (Maybe Text)
lglStartContinentCode = lens _lglStartContinentCode (\ s a -> s{_lglStartContinentCode = a});

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

instance Hashable ListGeoLocations

instance NFData ListGeoLocations

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

-- | A complex type that contains information about the geo locations that are returned by the request and information about the response.
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListGeoLocationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lglrsNextContinentCode'
--
-- * 'lglrsNextCountryCode'
--
-- * 'lglrsNextSubdivisionCode'
--
-- * 'lglrsResponseStatus'
--
-- * 'lglrsGeoLocationDetailsList'
--
-- * 'lglrsIsTruncated'
--
-- * 'lglrsMaxItems'
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

-- | If the results were truncated, the continent code of the next geo location in the list. This element is present only if < ListGeoLocationsResponse>IsTruncated> is true and the next geo location to list is a continent location.
lglrsNextContinentCode :: Lens' ListGeoLocationsResponse (Maybe Text)
lglrsNextContinentCode = lens _lglrsNextContinentCode (\ s a -> s{_lglrsNextContinentCode = a});

-- | If the results were truncated, the country code of the next geo location in the list. This element is present only if < ListGeoLocationsResponse>IsTruncated> is true and the next geo location to list is not a continent location.
lglrsNextCountryCode :: Lens' ListGeoLocationsResponse (Maybe Text)
lglrsNextCountryCode = lens _lglrsNextCountryCode (\ s a -> s{_lglrsNextCountryCode = a});

-- | If the results were truncated, the subdivision code of the next geo location in the list. This element is present only if < ListGeoLocationsResponse>IsTruncated> is true and the next geo location has a subdivision.
lglrsNextSubdivisionCode :: Lens' ListGeoLocationsResponse (Maybe Text)
lglrsNextSubdivisionCode = lens _lglrsNextSubdivisionCode (\ s a -> s{_lglrsNextSubdivisionCode = a});

-- | The response status code.
lglrsResponseStatus :: Lens' ListGeoLocationsResponse Int
lglrsResponseStatus = lens _lglrsResponseStatus (\ s a -> s{_lglrsResponseStatus = a});

-- | A complex type that contains information about the geo locations that are returned by the request.
lglrsGeoLocationDetailsList :: Lens' ListGeoLocationsResponse [GeoLocationDetails]
lglrsGeoLocationDetailsList = lens _lglrsGeoLocationDetailsList (\ s a -> s{_lglrsGeoLocationDetailsList = a}) . _Coerce;

-- | A flag that indicates whether there are more geo locations to be listed. If your results were truncated, you can make a follow-up request for the next page of results by using the values included in the < ListGeoLocationsResponse>NextContinentCode>, < ListGeoLocationsResponse>NextCountryCode> and < ListGeoLocationsResponse>NextSubdivisionCode> elements.
--
-- Valid Values: 'true' | 'false'
lglrsIsTruncated :: Lens' ListGeoLocationsResponse Bool
lglrsIsTruncated = lens _lglrsIsTruncated (\ s a -> s{_lglrsIsTruncated = a});

-- | The maximum number of records you requested. The maximum value of 'MaxItems' is 100.
lglrsMaxItems :: Lens' ListGeoLocationsResponse Text
lglrsMaxItems = lens _lglrsMaxItems (\ s a -> s{_lglrsMaxItems = a});

instance NFData ListGeoLocationsResponse
