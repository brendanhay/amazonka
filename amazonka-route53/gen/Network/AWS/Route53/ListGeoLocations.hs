{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.Route53.ListGeoLocations
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | To retrieve a list of supported geo locations, send a @GET@ request to
-- the @2013-04-01\/geolocations@ resource. The response to this request
-- includes a @GeoLocationDetailsList@ element with zero, one, or multiple
-- @GeoLocationDetails@ child elements. The list is sorted by country code,
-- and then subdivision code, followed by continents at the end of the
-- list.
--
-- By default, the list of geo locations is displayed on a single page. You
-- can control the length of the page that is displayed by using the
-- @MaxItems@ parameter. If the list is truncated, @IsTruncated@ will be
-- set to /true/ and a combination of
-- @NextContinentCode, NextCountryCode, NextSubdivisionCode@ will be
-- populated. You can pass these as parameters to
-- @StartContinentCode, StartCountryCode, StartSubdivisionCode@ to control
-- the geo location that the list begins with.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_ListGeoLocations.html>
module Network.AWS.Route53.ListGeoLocations
    (
    -- * Request
      ListGeoLocations
    -- ** Request constructor
    , listGeoLocations
    -- ** Request lenses
    , lglMaxItems
    , lglStartSubdivisionCode
    , lglStartCountryCode
    , lglStartContinentCode

    -- * Response
    , ListGeoLocationsResponse
    -- ** Response constructor
    , listGeoLocationsResponse
    -- ** Response lenses
    , lglrGeoLocationDetailsList
    , lglrIsTruncated
    , lglrMaxItems
    , lglrNextContinentCode
    , lglrNextCountryCode
    , lglrNextSubdivisionCode
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.Route53.Types

-- | /See:/ 'listGeoLocations' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lglMaxItems'
--
-- * 'lglStartSubdivisionCode'
--
-- * 'lglStartCountryCode'
--
-- * 'lglStartContinentCode'
data ListGeoLocations = ListGeoLocations'{_lglMaxItems :: Maybe Text, _lglStartSubdivisionCode :: Text, _lglStartCountryCode :: Text, _lglStartContinentCode :: Text} deriving (Eq, Read, Show)

-- | 'ListGeoLocations' smart constructor.
listGeoLocations :: Text -> Text -> Text -> ListGeoLocations
listGeoLocations pStartSubdivisionCode pStartCountryCode pStartContinentCode = ListGeoLocations'{_lglMaxItems = Nothing, _lglStartSubdivisionCode = pStartSubdivisionCode, _lglStartCountryCode = pStartCountryCode, _lglStartContinentCode = pStartContinentCode};

-- | The maximum number of geo locations you want in the response body.
lglMaxItems :: Lens' ListGeoLocations (Maybe Text)
lglMaxItems = lens _lglMaxItems (\ s a -> s{_lglMaxItems = a});

-- | The first subdivision code in the lexicographic ordering of geo
-- locations that you want the @ListGeoLocations@ request to list.
--
-- Constraint: Specifying @SubdivisionCode@ without @CountryCode@ returns
-- an InvalidInput error.
lglStartSubdivisionCode :: Lens' ListGeoLocations Text
lglStartSubdivisionCode = lens _lglStartSubdivisionCode (\ s a -> s{_lglStartSubdivisionCode = a});

-- | The first country code in the lexicographic ordering of geo locations
-- that you want the @ListGeoLocations@ request to list.
--
-- The default geo location uses a @*@ for the country code. All other
-- country codes follow the ISO 3166 two-character code.
lglStartCountryCode :: Lens' ListGeoLocations Text
lglStartCountryCode = lens _lglStartCountryCode (\ s a -> s{_lglStartCountryCode = a});

-- | The first continent code in the lexicographic ordering of geo locations
-- that you want the @ListGeoLocations@ request to list. For non-continent
-- geo locations, this should be null.
--
-- Valid values: @AF@ | @AN@ | @AS@ | @EU@ | @OC@ | @NA@ | @SA@
--
-- Constraint: Specifying @ContinentCode@ with either @CountryCode@ or
-- @SubdivisionCode@ returns an InvalidInput error.
lglStartContinentCode :: Lens' ListGeoLocations Text
lglStartContinentCode = lens _lglStartContinentCode (\ s a -> s{_lglStartContinentCode = a});

instance AWSRequest ListGeoLocations where
        type Sv ListGeoLocations = Route53
        type Rs ListGeoLocations = ListGeoLocationsResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 ListGeoLocationsResponse' <$>
                   (x .@? "GeoLocationDetailsList" .!@ mempty >>=
                      parseXMLList "GeoLocationDetails")
                     <*> x .@ "IsTruncated"
                     <*> x .@ "MaxItems"
                     <*> x .@ "NextContinentCode"
                     <*> x .@ "NextCountryCode"
                     <*> x .@ "NextSubdivisionCode")

instance ToHeaders ListGeoLocations where
        toHeaders = const mempty

instance ToPath ListGeoLocations where
        toPath = const "/2013-04-01/geolocations"

instance ToQuery ListGeoLocations where
        toQuery ListGeoLocations'{..}
          = mconcat
              ["maxitems" =: _lglMaxItems,
               "startsubdivisioncode" =: _lglStartSubdivisionCode,
               "startcountrycode" =: _lglStartCountryCode,
               "startcontinentcode" =: _lglStartContinentCode]

-- | /See:/ 'listGeoLocationsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lglrGeoLocationDetailsList'
--
-- * 'lglrIsTruncated'
--
-- * 'lglrMaxItems'
--
-- * 'lglrNextContinentCode'
--
-- * 'lglrNextCountryCode'
--
-- * 'lglrNextSubdivisionCode'
data ListGeoLocationsResponse = ListGeoLocationsResponse'{_lglrGeoLocationDetailsList :: [GeoLocationDetails], _lglrIsTruncated :: Bool, _lglrMaxItems :: Text, _lglrNextContinentCode :: Text, _lglrNextCountryCode :: Text, _lglrNextSubdivisionCode :: Text} deriving (Eq, Read, Show)

-- | 'ListGeoLocationsResponse' smart constructor.
listGeoLocationsResponse :: [GeoLocationDetails] -> Bool -> Text -> Text -> Text -> Text -> ListGeoLocationsResponse
listGeoLocationsResponse pGeoLocationDetailsList pIsTruncated pMaxItems pNextContinentCode pNextCountryCode pNextSubdivisionCode = ListGeoLocationsResponse'{_lglrGeoLocationDetailsList = pGeoLocationDetailsList, _lglrIsTruncated = pIsTruncated, _lglrMaxItems = pMaxItems, _lglrNextContinentCode = pNextContinentCode, _lglrNextCountryCode = pNextCountryCode, _lglrNextSubdivisionCode = pNextSubdivisionCode};

-- | A complex type that contains information about the geo locations that
-- are returned by the request.
lglrGeoLocationDetailsList :: Lens' ListGeoLocationsResponse [GeoLocationDetails]
lglrGeoLocationDetailsList = lens _lglrGeoLocationDetailsList (\ s a -> s{_lglrGeoLocationDetailsList = a});

-- | A flag that indicates whether there are more geo locations to be listed.
-- If your results were truncated, you can make a follow-up request for the
-- next page of results by using the values included in the
-- ListGeoLocationsResponse$NextContinentCode,
-- ListGeoLocationsResponse$NextCountryCode and
-- ListGeoLocationsResponse$NextSubdivisionCode elements.
--
-- Valid Values: @true@ | @false@
lglrIsTruncated :: Lens' ListGeoLocationsResponse Bool
lglrIsTruncated = lens _lglrIsTruncated (\ s a -> s{_lglrIsTruncated = a});

-- | The maximum number of records you requested. The maximum value of
-- @MaxItems@ is 100.
lglrMaxItems :: Lens' ListGeoLocationsResponse Text
lglrMaxItems = lens _lglrMaxItems (\ s a -> s{_lglrMaxItems = a});

-- | If the results were truncated, the continent code of the next geo
-- location in the list. This element is present only if
-- ListGeoLocationsResponse$IsTruncated is true and the next geo location
-- to list is a continent location.
lglrNextContinentCode :: Lens' ListGeoLocationsResponse Text
lglrNextContinentCode = lens _lglrNextContinentCode (\ s a -> s{_lglrNextContinentCode = a});

-- | If the results were truncated, the country code of the next geo location
-- in the list. This element is present only if
-- ListGeoLocationsResponse$IsTruncated is true and the next geo location
-- to list is not a continent location.
lglrNextCountryCode :: Lens' ListGeoLocationsResponse Text
lglrNextCountryCode = lens _lglrNextCountryCode (\ s a -> s{_lglrNextCountryCode = a});

-- | If the results were truncated, the subdivision code of the next geo
-- location in the list. This element is present only if
-- ListGeoLocationsResponse$IsTruncated is true and the next geo location
-- has a subdivision.
lglrNextSubdivisionCode :: Lens' ListGeoLocationsResponse Text
lglrNextSubdivisionCode = lens _lglrNextSubdivisionCode (\ s a -> s{_lglrNextSubdivisionCode = a});
