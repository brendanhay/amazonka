{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Route53.GetGeoLocation
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

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

import Network.AWS.Request.RestXML
import Network.AWS.Route53.Types
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
--
-- The fields accessible through corresponding lenses are:
--
-- * @ContinentCode ::@ @Maybe Text@
--
-- * @CountryCode ::@ @Maybe Text@
--
-- * @SubdivisionCode ::@ @Maybe Text@
--
getGeoLocation :: GetGeoLocation
getGeoLocation = GetGeoLocation
    { _gglContinentCode = Nothing
    , _gglCountryCode = Nothing
    , _gglSubdivisionCode = Nothing
    }

-- | The code for a continent geo location. Note: only continent locations have
-- a continent code. Valid values: AF | AN | AS | EU | OC | NA | SA
-- Constraint: Specifying ContinentCode with either CountryCode or
-- SubdivisionCode returns an InvalidInput error.
gglContinentCode :: Lens' GetGeoLocation (Maybe Text)
gglContinentCode =
    lens _gglContinentCode (\s a -> s { _gglContinentCode = a })

-- | The code for a country geo location. The default location uses '*' for the
-- country code and will match all locations that are not matched by a geo
-- location. The default geo location uses a * for the country code. All other
-- country codes follow the ISO 3166 two-character code.
gglCountryCode :: Lens' GetGeoLocation (Maybe Text)
gglCountryCode = lens _gglCountryCode (\s a -> s { _gglCountryCode = a })

-- | The code for a country's subdivision (e.g., a province of Canada). A
-- subdivision code is only valid with the appropriate country code.
-- Constraint: Specifying SubdivisionCode without CountryCode returns an
-- InvalidInput error.
gglSubdivisionCode :: Lens' GetGeoLocation (Maybe Text)
gglSubdivisionCode =
    lens _gglSubdivisionCode (\s a -> s { _gglSubdivisionCode = a })

instance ToPath GetGeoLocation

instance ToQuery GetGeoLocation

instance ToHeaders GetGeoLocation

instance ToXML GetGeoLocation where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "GetGeoLocationRequest"

-- | A complex type containing information about the specified geo location.
newtype GetGeoLocationResponse = GetGeoLocationResponse
    { _gglrGeoLocationDetails :: GeoLocationDetails
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'GetGeoLocationResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GeoLocationDetails ::@ @GeoLocationDetails@
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

instance AWSRequest GetGeoLocation where
    type Sv GetGeoLocation = Route53
    type Rs GetGeoLocation = GetGeoLocationResponse

    request = get
    response _ = xmlResponse
