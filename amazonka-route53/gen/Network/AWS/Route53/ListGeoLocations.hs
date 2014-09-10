{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Route53
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
    -- ** Response constructor
    , mkListGeoLocationsResponse
    -- ** Response lenses
    , lglrGeoLocationDetailsList
    , lglrIsTruncated
    , lglrNextContinentCode
    , lglrNextCountryCode
    , lglrNextSubdivisionCode
    , lglrMaxItems
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.Route53.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | The input for a ListGeoLocations request.
data ListGeoLocations = ListGeoLocations
    { _lglStartContinentCode :: !(Maybe Text)
    , _lglStartCountryCode :: !(Maybe Text)
    , _lglStartSubdivisionCode :: !(Maybe Text)
    , _lglMaxItems :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListGeoLocations' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StartContinentCode ::@ @Maybe Text@
--
-- * @StartCountryCode ::@ @Maybe Text@
--
-- * @StartSubdivisionCode ::@ @Maybe Text@
--
-- * @MaxItems ::@ @Maybe Text@
--
mkListGeoLocations :: ListGeoLocations
mkListGeoLocations = ListGeoLocations
    { _lglStartContinentCode = Nothing
    , _lglStartCountryCode = Nothing
    , _lglStartSubdivisionCode = Nothing
    , _lglMaxItems = Nothing
    }

-- | The first continent code in the lexicographic ordering of geo locations
-- that you want the ListGeoLocations request to list. For non-continent geo
-- locations, this should be null. Valid values: AF | AN | AS | EU | OC | NA |
-- SA Constraint: Specifying ContinentCode with either CountryCode or
-- SubdivisionCode returns an InvalidInput error.
lglStartContinentCode :: Lens' ListGeoLocations (Maybe Text)
lglStartContinentCode =
    lens _lglStartContinentCode (\s a -> s { _lglStartContinentCode = a })

-- | The first country code in the lexicographic ordering of geo locations that
-- you want the ListGeoLocations request to list. The default geo location
-- uses a * for the country code. All other country codes follow the ISO 3166
-- two-character code.
lglStartCountryCode :: Lens' ListGeoLocations (Maybe Text)
lglStartCountryCode =
    lens _lglStartCountryCode (\s a -> s { _lglStartCountryCode = a })

-- | The first subdivision code in the lexicographic ordering of geo locations
-- that you want the ListGeoLocations request to list. Constraint: Specifying
-- SubdivisionCode without CountryCode returns an InvalidInput error.
lglStartSubdivisionCode :: Lens' ListGeoLocations (Maybe Text)
lglStartSubdivisionCode =
    lens _lglStartSubdivisionCode
         (\s a -> s { _lglStartSubdivisionCode = a })

-- | The maximum number of geo locations you want in the response body.
lglMaxItems :: Lens' ListGeoLocations (Maybe Text)
lglMaxItems = lens _lglMaxItems (\s a -> s { _lglMaxItems = a })

instance ToPath ListGeoLocations

instance ToQuery ListGeoLocations

instance ToHeaders ListGeoLocations

instance ToXML ListGeoLocations where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ListGeoLocationsRequest"

-- | A complex type that contains information about the geo locations that are
-- returned by the request and information about the response.
data ListGeoLocationsResponse = ListGeoLocationsResponse
    { _lglrGeoLocationDetailsList :: [GeoLocationDetails]
    , _lglrIsTruncated :: !Bool
    , _lglrNextContinentCode :: !(Maybe Text)
    , _lglrNextCountryCode :: !(Maybe Text)
    , _lglrNextSubdivisionCode :: !(Maybe Text)
    , _lglrMaxItems :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListGeoLocationsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GeoLocationDetailsList ::@ @[GeoLocationDetails]@
--
-- * @IsTruncated ::@ @Bool@
--
-- * @NextContinentCode ::@ @Maybe Text@
--
-- * @NextCountryCode ::@ @Maybe Text@
--
-- * @NextSubdivisionCode ::@ @Maybe Text@
--
-- * @MaxItems ::@ @Text@
--
mkListGeoLocationsResponse :: [GeoLocationDetails] -- ^ 'lglrGeoLocationDetailsList'
                           -> Bool -- ^ 'lglrIsTruncated'
                           -> Text -- ^ 'lglrMaxItems'
                           -> ListGeoLocationsResponse
mkListGeoLocationsResponse p1 p2 p6 = ListGeoLocationsResponse
    { _lglrGeoLocationDetailsList = p1
    , _lglrIsTruncated = p2
    , _lglrNextContinentCode = Nothing
    , _lglrNextCountryCode = Nothing
    , _lglrNextSubdivisionCode = Nothing
    , _lglrMaxItems = p6
    }

-- | A complex type that contains information about the geo locations that are
-- returned by the request.
lglrGeoLocationDetailsList :: Lens' ListGeoLocationsResponse [GeoLocationDetails]
lglrGeoLocationDetailsList =
    lens _lglrGeoLocationDetailsList
         (\s a -> s { _lglrGeoLocationDetailsList = a })

-- | A flag that indicates whether there are more geo locations to be listed. If
-- your results were truncated, you can make a follow-up request for the next
-- page of results by using the values included in the
-- ListGeoLocationsResponse$NextContinentCode,
-- ListGeoLocationsResponse$NextCountryCode and
-- ListGeoLocationsResponse$NextSubdivisionCode elements. Valid Values: true |
-- false.
lglrIsTruncated :: Lens' ListGeoLocationsResponse Bool
lglrIsTruncated = lens _lglrIsTruncated (\s a -> s { _lglrIsTruncated = a })

-- | If the results were truncated, the continent code of the next geo location
-- in the list. This element is present only if
-- ListGeoLocationsResponse$IsTruncated is true and the next geo location to
-- list is a continent location.
lglrNextContinentCode :: Lens' ListGeoLocationsResponse (Maybe Text)
lglrNextContinentCode =
    lens _lglrNextContinentCode (\s a -> s { _lglrNextContinentCode = a })

-- | If the results were truncated, the country code of the next geo location in
-- the list. This element is present only if
-- ListGeoLocationsResponse$IsTruncated is true and the next geo location to
-- list is not a continent location.
lglrNextCountryCode :: Lens' ListGeoLocationsResponse (Maybe Text)
lglrNextCountryCode =
    lens _lglrNextCountryCode (\s a -> s { _lglrNextCountryCode = a })

-- | If the results were truncated, the subdivision code of the next geo
-- location in the list. This element is present only if
-- ListGeoLocationsResponse$IsTruncated is true and the next geo location has
-- a subdivision.
lglrNextSubdivisionCode :: Lens' ListGeoLocationsResponse (Maybe Text)
lglrNextSubdivisionCode =
    lens _lglrNextSubdivisionCode
         (\s a -> s { _lglrNextSubdivisionCode = a })

-- | The maximum number of records you requested. The maximum value of MaxItems
-- is 100.
lglrMaxItems :: Lens' ListGeoLocationsResponse Text
lglrMaxItems = lens _lglrMaxItems (\s a -> s { _lglrMaxItems = a })

instance FromXML ListGeoLocationsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListGeoLocations where
    type Sv ListGeoLocations = Route53
    type Rs ListGeoLocations = ListGeoLocationsResponse

    request = get
    response _ = xmlResponse
