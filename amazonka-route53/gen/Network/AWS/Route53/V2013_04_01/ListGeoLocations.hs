{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53.V2013_04_01.ListGeoLocations
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Route53.V2013_04_01.ListGeoLocations
    (
    -- * Request
      ListGeoLocations
    -- ** Request constructor
    , mkListGeoLocations
    -- ** Request lenses
    , lglStartContinentCode
    , lglStartCountryCode
    , lglStartSubdivisionCode
    , lglMaxItems

    -- * Response
    , ListGeoLocationsResponse
    -- ** Response lenses
    , lglrsGeoLocationDetailsList
    , lglrsIsTruncated
    , lglrsNextContinentCode
    , lglrsNextCountryCode
    , lglrsNextSubdivisionCode
    , lglrsMaxItems
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.Route53.V2013_04_01.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | The input for a ListGeoLocations request.
data ListGeoLocations = ListGeoLocations
    { _lglStartContinentCode :: Maybe Text
    , _lglStartCountryCode :: Maybe Text
    , _lglStartSubdivisionCode :: Maybe Text
    , _lglMaxItems :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListGeoLocations' request.
mkListGeoLocations :: ListGeoLocations
mkListGeoLocations = ListGeoLocations
    { _lglStartContinentCode = Nothing
    , _lglStartCountryCode = Nothing
    , _lglStartSubdivisionCode = Nothing
    , _lglMaxItems = Nothing
    }
{-# INLINE mkListGeoLocations #-}

-- | The first continent code in the lexicographic ordering of geo locations
-- that you want the ListGeoLocations request to list. For non-continent geo
-- locations, this should be null. Valid values: AF | AN | AS | EU | OC | NA |
-- SA Constraint: Specifying ContinentCode with either CountryCode or
-- SubdivisionCode returns an InvalidInput error.
lglStartContinentCode :: Lens' ListGeoLocations (Maybe Text)
lglStartContinentCode =
    lens _lglStartContinentCode (\s a -> s { _lglStartContinentCode = a })
{-# INLINE lglStartContinentCode #-}

-- | The first country code in the lexicographic ordering of geo locations that
-- you want the ListGeoLocations request to list. The default geo location
-- uses a * for the country code. All other country codes follow the ISO 3166
-- two-character code.
lglStartCountryCode :: Lens' ListGeoLocations (Maybe Text)
lglStartCountryCode =
    lens _lglStartCountryCode (\s a -> s { _lglStartCountryCode = a })
{-# INLINE lglStartCountryCode #-}

-- | The first subdivision code in the lexicographic ordering of geo locations
-- that you want the ListGeoLocations request to list. Constraint: Specifying
-- SubdivisionCode without CountryCode returns an InvalidInput error.
lglStartSubdivisionCode :: Lens' ListGeoLocations (Maybe Text)
lglStartSubdivisionCode =
    lens _lglStartSubdivisionCode
         (\s a -> s { _lglStartSubdivisionCode = a })
{-# INLINE lglStartSubdivisionCode #-}

-- | The maximum number of geo locations you want in the response body.
lglMaxItems :: Lens' ListGeoLocations (Maybe Text)
lglMaxItems = lens _lglMaxItems (\s a -> s { _lglMaxItems = a })
{-# INLINE lglMaxItems #-}

instance ToPath ListGeoLocations where
    toPath = const "/2013-04-01/geolocations"

instance ToQuery ListGeoLocations where
    toQuery ListGeoLocations{..} = mconcat
        [ "maxitems" =? _lglMaxItems
        , "startcontinentcode" =? _lglStartContinentCode
        , "startcountrycode" =? _lglStartCountryCode
        , "startsubdivisioncode" =? _lglStartSubdivisionCode
        ]

instance ToHeaders ListGeoLocations

instance ToXML ListGeoLocations where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ListGeoLocationsRequest"

-- | A complex type that contains information about the geo locations that are
-- returned by the request and information about the response.
data ListGeoLocationsResponse = ListGeoLocationsResponse
    { _lglrsGeoLocationDetailsList :: [GeoLocationDetails]
    , _lglrsIsTruncated :: Bool
    , _lglrsNextContinentCode :: Maybe Text
    , _lglrsNextCountryCode :: Maybe Text
    , _lglrsNextSubdivisionCode :: Maybe Text
    , _lglrsMaxItems :: Text
    } deriving (Show, Generic)

-- | A complex type that contains information about the geo locations that are
-- returned by the request.
lglrsGeoLocationDetailsList :: Lens' ListGeoLocationsResponse [GeoLocationDetails]
lglrsGeoLocationDetailsList =
    lens _lglrsGeoLocationDetailsList
         (\s a -> s { _lglrsGeoLocationDetailsList = a })
{-# INLINE lglrsGeoLocationDetailsList #-}

-- | A flag that indicates whether there are more geo locations to be listed. If
-- your results were truncated, you can make a follow-up request for the next
-- page of results by using the values included in the
-- ListGeoLocationsResponse$NextContinentCode,
-- ListGeoLocationsResponse$NextCountryCode and
-- ListGeoLocationsResponse$NextSubdivisionCode elements. Valid Values: true |
-- false.
lglrsIsTruncated :: Lens' ListGeoLocationsResponse Bool
lglrsIsTruncated =
    lens _lglrsIsTruncated (\s a -> s { _lglrsIsTruncated = a })
{-# INLINE lglrsIsTruncated #-}

-- | If the results were truncated, the continent code of the next geo location
-- in the list. This element is present only if
-- ListGeoLocationsResponse$IsTruncated is true and the next geo location to
-- list is a continent location.
lglrsNextContinentCode :: Lens' ListGeoLocationsResponse (Maybe Text)
lglrsNextContinentCode =
    lens _lglrsNextContinentCode (\s a -> s { _lglrsNextContinentCode = a })
{-# INLINE lglrsNextContinentCode #-}

-- | If the results were truncated, the country code of the next geo location in
-- the list. This element is present only if
-- ListGeoLocationsResponse$IsTruncated is true and the next geo location to
-- list is not a continent location.
lglrsNextCountryCode :: Lens' ListGeoLocationsResponse (Maybe Text)
lglrsNextCountryCode =
    lens _lglrsNextCountryCode (\s a -> s { _lglrsNextCountryCode = a })
{-# INLINE lglrsNextCountryCode #-}

-- | If the results were truncated, the subdivision code of the next geo
-- location in the list. This element is present only if
-- ListGeoLocationsResponse$IsTruncated is true and the next geo location has
-- a subdivision.
lglrsNextSubdivisionCode :: Lens' ListGeoLocationsResponse (Maybe Text)
lglrsNextSubdivisionCode =
    lens _lglrsNextSubdivisionCode
         (\s a -> s { _lglrsNextSubdivisionCode = a })
{-# INLINE lglrsNextSubdivisionCode #-}

-- | The maximum number of records you requested. The maximum value of MaxItems
-- is 100.
lglrsMaxItems :: Lens' ListGeoLocationsResponse Text
lglrsMaxItems = lens _lglrsMaxItems (\s a -> s { _lglrsMaxItems = a })
{-# INLINE lglrsMaxItems #-}

instance FromXML ListGeoLocationsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListGeoLocations where
    type Sv ListGeoLocations = Route53
    type Rs ListGeoLocations = ListGeoLocationsResponse

    request = get
    response _ = xmlResponse
