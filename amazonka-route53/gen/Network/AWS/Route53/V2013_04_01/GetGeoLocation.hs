{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53.V2013_04_01.GetGeoLocation
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Route53.V2013_04_01.GetGeoLocation
    (
    -- * Request
      GetGeoLocation
    -- ** Request constructor
    , mkGetGeoLocation
    -- ** Request lenses
    , gglContinentCode
    , gglCountryCode
    , gglSubdivisionCode

    -- * Response
    , GetGeoLocationResponse
    -- ** Response lenses
    , gglrsGeoLocationDetails
    ) where

import Network.AWS.Request.RestXML
import Network.AWS.Route53.V2013_04_01.Types
import Network.AWS.Prelude
import Network.AWS.Types (Region)

-- | A complex type that contains information about the request to get a geo
-- location.
data GetGeoLocation = GetGeoLocation
    { _gglContinentCode :: Maybe Text
    , _gglCountryCode :: Maybe Text
    , _gglSubdivisionCode :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetGeoLocation' request.
mkGetGeoLocation :: GetGeoLocation
mkGetGeoLocation = GetGeoLocation
    { _gglContinentCode = Nothing
    , _gglCountryCode = Nothing
    , _gglSubdivisionCode = Nothing
    }
{-# INLINE mkGetGeoLocation #-}

-- | The code for a continent geo location. Note: only continent locations have
-- a continent code. Valid values: AF | AN | AS | EU | OC | NA | SA
-- Constraint: Specifying ContinentCode with either CountryCode or
-- SubdivisionCode returns an InvalidInput error.
gglContinentCode :: Lens' GetGeoLocation (Maybe Text)
gglContinentCode =
    lens _gglContinentCode (\s a -> s { _gglContinentCode = a })
{-# INLINE gglContinentCode #-}

-- | The code for a country geo location. The default location uses '*' for the
-- country code and will match all locations that are not matched by a geo
-- location. The default geo location uses a * for the country code. All other
-- country codes follow the ISO 3166 two-character code.
gglCountryCode :: Lens' GetGeoLocation (Maybe Text)
gglCountryCode = lens _gglCountryCode (\s a -> s { _gglCountryCode = a })
{-# INLINE gglCountryCode #-}

-- | The code for a country's subdivision (e.g., a province of Canada). A
-- subdivision code is only valid with the appropriate country code.
-- Constraint: Specifying SubdivisionCode without CountryCode returns an
-- InvalidInput error.
gglSubdivisionCode :: Lens' GetGeoLocation (Maybe Text)
gglSubdivisionCode =
    lens _gglSubdivisionCode (\s a -> s { _gglSubdivisionCode = a })
{-# INLINE gglSubdivisionCode #-}

instance ToPath GetGeoLocation where
    toPath = const "/2013-04-01/geolocation"

instance ToQuery GetGeoLocation where
    toQuery GetGeoLocation{..} = mconcat
        [ "continentcode" =? _gglContinentCode
        , "countrycode" =? _gglCountryCode
        , "subdivisioncode" =? _gglSubdivisionCode
        ]

instance ToHeaders GetGeoLocation

instance ToXML GetGeoLocation where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "GetGeoLocationRequest"

-- | A complex type containing information about the specified geo location.
newtype GetGeoLocationResponse = GetGeoLocationResponse
    { _gglrsGeoLocationDetails :: GeoLocationDetails
    } deriving (Show, Generic)

-- | A complex type that contains the information about the specified geo
-- location.
gglrsGeoLocationDetails :: Lens' GetGeoLocationResponse GeoLocationDetails
gglrsGeoLocationDetails =
    lens _gglrsGeoLocationDetails
         (\s a -> s { _gglrsGeoLocationDetails = a })
{-# INLINE gglrsGeoLocationDetails #-}

instance FromXML GetGeoLocationResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetGeoLocation where
    type Sv GetGeoLocation = Route53
    type Rs GetGeoLocation = GetGeoLocationResponse

    request = get
    response _ = xmlResponse
