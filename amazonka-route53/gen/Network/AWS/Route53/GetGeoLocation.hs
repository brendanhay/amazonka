{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.Route53.GetGeoLocation
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | To retrieve a single geo location, send a GET request to the
-- 2013-04-01/geolocation resource with one of these options: continentcode |
-- countrycode | countrycode and subdivisioncode.
module Network.AWS.Route53.GetGeoLocation
    (
    -- * Request
      GetGeoLocation
    -- ** Request constructor
    , getGeoLocation
    -- ** Request lenses
    , gglContinentCode
    , gglCountryCode
    , gglSubdivisionCode

    -- * Response
    , GetGeoLocationResponse
    -- ** Response constructor
    , getGeoLocationResponse
    -- ** Response lenses
    , gglrGeoLocationDetails
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Route53.Types

data GetGeoLocation = GetGeoLocation
    { _gglContinentCode   :: Maybe Text
    , _gglCountryCode     :: Maybe Text
    , _gglSubdivisionCode :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'GetGeoLocation' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gglContinentCode' @::@ 'Maybe' 'Text'
--
-- * 'gglCountryCode' @::@ 'Maybe' 'Text'
--
-- * 'gglSubdivisionCode' @::@ 'Maybe' 'Text'
--
getGeoLocation :: GetGeoLocation
getGeoLocation = GetGeoLocation
    { _gglContinentCode   = Nothing
    , _gglCountryCode     = Nothing
    , _gglSubdivisionCode = Nothing
    }

-- | The code for a continent geo location. Note: only continent locations
-- have a continent code. Valid values: AF | AN | AS | EU | OC | NA | SA
-- Constraint: Specifying ContinentCode with either CountryCode or
-- SubdivisionCode returns an InvalidInput error.
gglContinentCode :: Lens' GetGeoLocation (Maybe Text)
gglContinentCode = lens _gglContinentCode (\s a -> s { _gglContinentCode = a })

-- | The code for a country geo location. The default location uses '*' for
-- the country code and will match all locations that are not matched by a
-- geo location. The default geo location uses a * for the country code. All
-- other country codes follow the ISO 3166 two-character code.
gglCountryCode :: Lens' GetGeoLocation (Maybe Text)
gglCountryCode = lens _gglCountryCode (\s a -> s { _gglCountryCode = a })

-- | The code for a country's subdivision (e.g., a province of Canada). A
-- subdivision code is only valid with the appropriate country code.
-- Constraint: Specifying SubdivisionCode without CountryCode returns an
-- InvalidInput error.
gglSubdivisionCode :: Lens' GetGeoLocation (Maybe Text)
gglSubdivisionCode =
    lens _gglSubdivisionCode (\s a -> s { _gglSubdivisionCode = a })

instance ToPath GetGeoLocation where
    toPath = const "/2013-04-01/geolocation"

instance ToQuery GetGeoLocation where
    toQuery GetGeoLocation{..} = mconcat
        [ "continentcode"   =? _gglContinentCode
        , "countrycode"     =? _gglCountryCode
        , "subdivisioncode" =? _gglSubdivisionCode
        ]

instance ToHeaders GetGeoLocation

newtype GetGeoLocationResponse = GetGeoLocationResponse
    { _gglrGeoLocationDetails :: GeoLocationDetails
    } deriving (Eq, Show, Generic)

-- | 'GetGeoLocationResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gglrGeoLocationDetails' @::@ 'GeoLocationDetails'
--
getGeoLocationResponse :: GeoLocationDetails -- ^ 'gglrGeoLocationDetails'
                       -> GetGeoLocationResponse
getGeoLocationResponse p1 = GetGeoLocationResponse
    { _gglrGeoLocationDetails = p1
    }

-- | A complex type that contains the information about the specified geo
-- location.
gglrGeoLocationDetails :: Lens' GetGeoLocationResponse GeoLocationDetails
gglrGeoLocationDetails =
    lens _gglrGeoLocationDetails (\s a -> s { _gglrGeoLocationDetails = a })

instance FromXML GetGeoLocationResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "GetGeoLocationResponse"
instance AWSRequest GetGeoLocation where
    type Sv GetGeoLocation = Route53
    type Rs GetGeoLocation = GetGeoLocationResponse

    request  = get
    response = xmlResponse $ \h x -> GetGeoLocationResponse
        <$> x %| "GeoLocationDetails"
