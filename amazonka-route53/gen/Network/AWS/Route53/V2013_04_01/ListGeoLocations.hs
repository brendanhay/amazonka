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
    , listGeoLocations
    -- ** Request lenses
    , lglrStartContinentCode
    , lglrStartCountryCode
    , lglrStartSubdivisionCode
    , lglrMaxItems

    -- * Response
    , ListGeoLocationsResponse
    -- ** Response lenses
    , lglsGeoLocationDetailsList
    , lglsMaxItems
    , lglsIsTruncated
    , lglsNextContinentCode
    , lglsNextCountryCode
    , lglsNextSubdivisionCode
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.Route53.V2013_04_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ListGeoLocations' request.
listGeoLocations :: ListGeoLocations
listGeoLocations = ListGeoLocations
    { _lglrStartContinentCode = Nothing
    , _lglrStartCountryCode = Nothing
    , _lglrStartSubdivisionCode = Nothing
    , _lglrMaxItems = Nothing
    }

data ListGeoLocations = ListGeoLocations
    { _lglrStartContinentCode :: Maybe Text
      -- ^ The first continent code in the lexicographic ordering of geo
      -- locations that you want the ListGeoLocations request to list. For
      -- non-continent geo locations, this should be null. Valid values:
      -- AF | AN | AS | EU | OC | NA | SA Constraint: Specifying
      -- ContinentCode with either CountryCode or SubdivisionCode returns
      -- an InvalidInput error.
    , _lglrStartCountryCode :: Maybe Text
      -- ^ The first country code in the lexicographic ordering of geo
      -- locations that you want the ListGeoLocations request to list. The
      -- default geo location uses a * for the country code. All other
      -- country codes follow the ISO 3166 two-character code.
    , _lglrStartSubdivisionCode :: Maybe Text
      -- ^ The first subdivision code in the lexicographic ordering of geo
      -- locations that you want the ListGeoLocations request to list.
      -- Constraint: Specifying SubdivisionCode without CountryCode
      -- returns an InvalidInput error.
    , _lglrMaxItems :: Maybe Text
      -- ^ The maximum number of geo locations you want in the response
      -- body.
    } deriving (Show, Generic)

-- | The first continent code in the lexicographic ordering of geo locations
-- that you want the ListGeoLocations request to list. For non-continent geo
-- locations, this should be null. Valid values: AF | AN | AS | EU | OC | NA |
-- SA Constraint: Specifying ContinentCode with either CountryCode or
-- SubdivisionCode returns an InvalidInput error.
lglrStartContinentCode
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListGeoLocations
    -> f ListGeoLocations
lglrStartContinentCode f x =
    (\y -> x { _lglrStartContinentCode = y })
       <$> f (_lglrStartContinentCode x)
{-# INLINE lglrStartContinentCode #-}

-- | The first country code in the lexicographic ordering of geo locations that
-- you want the ListGeoLocations request to list. The default geo location
-- uses a * for the country code. All other country codes follow the ISO 3166
-- two-character code.
lglrStartCountryCode
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListGeoLocations
    -> f ListGeoLocations
lglrStartCountryCode f x =
    (\y -> x { _lglrStartCountryCode = y })
       <$> f (_lglrStartCountryCode x)
{-# INLINE lglrStartCountryCode #-}

-- | The first subdivision code in the lexicographic ordering of geo locations
-- that you want the ListGeoLocations request to list. Constraint: Specifying
-- SubdivisionCode without CountryCode returns an InvalidInput error.
lglrStartSubdivisionCode
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListGeoLocations
    -> f ListGeoLocations
lglrStartSubdivisionCode f x =
    (\y -> x { _lglrStartSubdivisionCode = y })
       <$> f (_lglrStartSubdivisionCode x)
{-# INLINE lglrStartSubdivisionCode #-}

-- | The maximum number of geo locations you want in the response body.
lglrMaxItems
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListGeoLocations
    -> f ListGeoLocations
lglrMaxItems f x =
    (\y -> x { _lglrMaxItems = y })
       <$> f (_lglrMaxItems x)
{-# INLINE lglrMaxItems #-}

instance ToPath ListGeoLocations where
    toPath = const "/2013-04-01/geolocations"

instance ToQuery ListGeoLocations where
    toQuery ListGeoLocations{..} = mconcat
        [ "maxitems" =? _lglrMaxItems
        , "startcontinentcode" =? _lglrStartContinentCode
        , "startcountrycode" =? _lglrStartCountryCode
        , "startsubdivisioncode" =? _lglrStartSubdivisionCode
        ]

instance ToHeaders ListGeoLocations

instance ToXML ListGeoLocations where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "ListGeoLocationsRequest"

data ListGeoLocationsResponse = ListGeoLocationsResponse
    { _lglsGeoLocationDetailsList :: [GeoLocationDetails]
      -- ^ A complex type that contains information about the geo locations
      -- that are returned by the request.
    , _lglsMaxItems :: Text
      -- ^ The maximum number of records you requested. The maximum value of
      -- MaxItems is 100.
    , _lglsIsTruncated :: Bool
      -- ^ A flag that indicates whether there are more geo locations to be
      -- listed. If your results were truncated, you can make a follow-up
      -- request for the next page of results by using the values included
      -- in the ListGeoLocationsResponse$NextContinentCode,
      -- ListGeoLocationsResponse$NextCountryCode and
      -- ListGeoLocationsResponse$NextSubdivisionCode elements. Valid
      -- Values: true | false.
    , _lglsNextContinentCode :: Maybe Text
      -- ^ If the results were truncated, the continent code of the next geo
      -- location in the list. This element is present only if
      -- ListGeoLocationsResponse$IsTruncated is true and the next geo
      -- location to list is a continent location.
    , _lglsNextCountryCode :: Maybe Text
      -- ^ If the results were truncated, the country code of the next geo
      -- location in the list. This element is present only if
      -- ListGeoLocationsResponse$IsTruncated is true and the next geo
      -- location to list is not a continent location.
    , _lglsNextSubdivisionCode :: Maybe Text
      -- ^ If the results were truncated, the subdivision code of the next
      -- geo location in the list. This element is present only if
      -- ListGeoLocationsResponse$IsTruncated is true and the next geo
      -- location has a subdivision.
    } deriving (Show, Generic)

-- | A complex type that contains information about the geo locations that are
-- returned by the request.
lglsGeoLocationDetailsList
    :: Functor f
    => ([GeoLocationDetails]
    -> f ([GeoLocationDetails]))
    -> ListGeoLocationsResponse
    -> f ListGeoLocationsResponse
lglsGeoLocationDetailsList f x =
    (\y -> x { _lglsGeoLocationDetailsList = y })
       <$> f (_lglsGeoLocationDetailsList x)
{-# INLINE lglsGeoLocationDetailsList #-}

-- | The maximum number of records you requested. The maximum value of MaxItems
-- is 100.
lglsMaxItems
    :: Functor f
    => (Text
    -> f (Text))
    -> ListGeoLocationsResponse
    -> f ListGeoLocationsResponse
lglsMaxItems f x =
    (\y -> x { _lglsMaxItems = y })
       <$> f (_lglsMaxItems x)
{-# INLINE lglsMaxItems #-}

-- | A flag that indicates whether there are more geo locations to be listed. If
-- your results were truncated, you can make a follow-up request for the next
-- page of results by using the values included in the
-- ListGeoLocationsResponse$NextContinentCode,
-- ListGeoLocationsResponse$NextCountryCode and
-- ListGeoLocationsResponse$NextSubdivisionCode elements. Valid Values: true |
-- false.
lglsIsTruncated
    :: Functor f
    => (Bool
    -> f (Bool))
    -> ListGeoLocationsResponse
    -> f ListGeoLocationsResponse
lglsIsTruncated f x =
    (\y -> x { _lglsIsTruncated = y })
       <$> f (_lglsIsTruncated x)
{-# INLINE lglsIsTruncated #-}

-- | If the results were truncated, the continent code of the next geo location
-- in the list. This element is present only if
-- ListGeoLocationsResponse$IsTruncated is true and the next geo location to
-- list is a continent location.
lglsNextContinentCode
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListGeoLocationsResponse
    -> f ListGeoLocationsResponse
lglsNextContinentCode f x =
    (\y -> x { _lglsNextContinentCode = y })
       <$> f (_lglsNextContinentCode x)
{-# INLINE lglsNextContinentCode #-}

-- | If the results were truncated, the country code of the next geo location in
-- the list. This element is present only if
-- ListGeoLocationsResponse$IsTruncated is true and the next geo location to
-- list is not a continent location.
lglsNextCountryCode
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListGeoLocationsResponse
    -> f ListGeoLocationsResponse
lglsNextCountryCode f x =
    (\y -> x { _lglsNextCountryCode = y })
       <$> f (_lglsNextCountryCode x)
{-# INLINE lglsNextCountryCode #-}

-- | If the results were truncated, the subdivision code of the next geo
-- location in the list. This element is present only if
-- ListGeoLocationsResponse$IsTruncated is true and the next geo location has
-- a subdivision.
lglsNextSubdivisionCode
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ListGeoLocationsResponse
    -> f ListGeoLocationsResponse
lglsNextSubdivisionCode f x =
    (\y -> x { _lglsNextSubdivisionCode = y })
       <$> f (_lglsNextSubdivisionCode x)
{-# INLINE lglsNextSubdivisionCode #-}

instance FromXML ListGeoLocationsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListGeoLocations where
    type Sv ListGeoLocations = Route53
    type Rs ListGeoLocations = ListGeoLocationsResponse

    request = get
    response _ = xmlResponse
