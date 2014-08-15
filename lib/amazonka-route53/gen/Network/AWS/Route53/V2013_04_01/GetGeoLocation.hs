{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- Module      : Network.AWS.Route53.V2013_04_01.GetGeoLocation
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Route53.V2013_04_01.GetGeoLocation where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.RestXML
import Network.AWS.Route53.V2013_04_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'GetGeoLocation' request.
getGeoLocation :: GetGeoLocation
getGeoLocation = GetGeoLocation
    { _gglrContinentCode = Nothing
    , _gglrCountryCode = Nothing
    , _gglrSubdivisionCode = Nothing
    }

data GetGeoLocation = GetGeoLocation
    { _gglrContinentCode :: Maybe Text
      -- ^ The code for a continent geo location. Note: only continent
      -- locations have a continent code. Valid values: AF | AN | AS | EU
      -- | OC | NA | SA Constraint: Specifying ContinentCode with either
      -- CountryCode or SubdivisionCode returns an InvalidInput error.
    , _gglrCountryCode :: Maybe Text
      -- ^ The code for a country geo location. The default location uses
      -- '*' for the country code and will match all locations that are
      -- not matched by a geo location. The default geo location uses a *
      -- for the country code. All other country codes follow the ISO 3166
      -- two-character code.
    , _gglrSubdivisionCode :: Maybe Text
      -- ^ The code for a country's subdivision (e.g., a province of
      -- Canada). A subdivision code is only valid with the appropriate
      -- country code. Constraint: Specifying SubdivisionCode without
      -- CountryCode returns an InvalidInput error.
    } deriving (Show, Generic)

makeLenses ''GetGeoLocation

instance ToPath GetGeoLocation where
    toPath = const "/2013-04-01/geolocation"

instance ToQuery GetGeoLocation where
    toQuery GetGeoLocation{..} = mconcat
        [ "continentcode" =? _gglrContinentCode
        , "countrycode" =? _gglrCountryCode
        , "subdivisioncode" =? _gglrSubdivisionCode
        ]

instance ToHeaders GetGeoLocation

instance ToXML GetGeoLocation where
    toXMLOptions = xmlOptions
    toXMLRoot    = toRoot "GetGeoLocationRequest"

data GetGeoLocationResponse = GetGeoLocationResponse
    { _gglsGeoLocationDetails :: GeoLocationDetails
      -- ^ A complex type that contains the information about the specified
      -- geo location.
    } deriving (Show, Generic)

makeLenses ''GetGeoLocationResponse

instance FromXML GetGeoLocationResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetGeoLocation where
    type Sv GetGeoLocation = Route53
    type Rs GetGeoLocation = GetGeoLocationResponse

    request = get
    response _ = xmlResponse
