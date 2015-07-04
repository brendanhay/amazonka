{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.Route53.GetGeoLocation
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | To retrieve a single geo location, send a @GET@ request to the
-- @2013-04-01\/geolocation@ resource with one of these options:
-- continentcode | countrycode | countrycode and subdivisioncode.
--
-- <http://docs.aws.amazon.com/Route53/latest/APIReference/API_GetGeoLocation.html>
module Network.AWS.Route53.GetGeoLocation
    (
    -- * Request
      GetGeoLocation
    -- ** Request constructor
    , getGeoLocation
    -- ** Request lenses
    , gglSubdivisionCode
    , gglCountryCode
    , gglContinentCode

    -- * Response
    , GetGeoLocationResponse
    -- ** Response constructor
    , getGeoLocationResponse
    -- ** Response lenses
    , gglrStatus
    , gglrGeoLocationDetails
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types

-- | A complex type that contains information about the request to get a geo
-- location.
--
-- /See:/ 'getGeoLocation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gglSubdivisionCode'
--
-- * 'gglCountryCode'
--
-- * 'gglContinentCode'
data GetGeoLocation = GetGeoLocation'
    { _gglSubdivisionCode :: !(Maybe Text)
    , _gglCountryCode     :: !(Maybe Text)
    , _gglContinentCode   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetGeoLocation' smart constructor.
getGeoLocation :: GetGeoLocation
getGeoLocation =
    GetGeoLocation'
    { _gglSubdivisionCode = Nothing
    , _gglCountryCode = Nothing
    , _gglContinentCode = Nothing
    }

-- | The code for a country\'s subdivision (e.g., a province of Canada). A
-- subdivision code is only valid with the appropriate country code.
--
-- Constraint: Specifying @SubdivisionCode@ without @CountryCode@ returns
-- an InvalidInput error.
gglSubdivisionCode :: Lens' GetGeoLocation (Maybe Text)
gglSubdivisionCode = lens _gglSubdivisionCode (\ s a -> s{_gglSubdivisionCode = a});

-- | The code for a country geo location. The default location uses \'*\' for
-- the country code and will match all locations that are not matched by a
-- geo location.
--
-- The default geo location uses a @*@ for the country code. All other
-- country codes follow the ISO 3166 two-character code.
gglCountryCode :: Lens' GetGeoLocation (Maybe Text)
gglCountryCode = lens _gglCountryCode (\ s a -> s{_gglCountryCode = a});

-- | The code for a continent geo location. Note: only continent locations
-- have a continent code.
--
-- Valid values: @AF@ | @AN@ | @AS@ | @EU@ | @OC@ | @NA@ | @SA@
--
-- Constraint: Specifying @ContinentCode@ with either @CountryCode@ or
-- @SubdivisionCode@ returns an InvalidInput error.
gglContinentCode :: Lens' GetGeoLocation (Maybe Text)
gglContinentCode = lens _gglContinentCode (\ s a -> s{_gglContinentCode = a});

instance AWSRequest GetGeoLocation where
        type Sv GetGeoLocation = Route53
        type Rs GetGeoLocation = GetGeoLocationResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 GetGeoLocationResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "GeoLocationDetails"))

instance ToHeaders GetGeoLocation where
        toHeaders = const mempty

instance ToPath GetGeoLocation where
        toPath = const "/2013-04-01/geolocation"

instance ToQuery GetGeoLocation where
        toQuery GetGeoLocation'{..}
          = mconcat
              ["subdivisioncode" =: _gglSubdivisionCode,
               "countrycode" =: _gglCountryCode,
               "continentcode" =: _gglContinentCode]

-- | A complex type containing information about the specified geo location.
--
-- /See:/ 'getGeoLocationResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gglrStatus'
--
-- * 'gglrGeoLocationDetails'
data GetGeoLocationResponse = GetGeoLocationResponse'
    { _gglrStatus             :: !Int
    , _gglrGeoLocationDetails :: !GeoLocationDetails
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetGeoLocationResponse' smart constructor.
getGeoLocationResponse :: Int -> GeoLocationDetails -> GetGeoLocationResponse
getGeoLocationResponse pStatus pGeoLocationDetails =
    GetGeoLocationResponse'
    { _gglrStatus = pStatus
    , _gglrGeoLocationDetails = pGeoLocationDetails
    }

-- | FIXME: Undocumented member.
gglrStatus :: Lens' GetGeoLocationResponse Int
gglrStatus = lens _gglrStatus (\ s a -> s{_gglrStatus = a});

-- | A complex type that contains the information about the specified geo
-- location.
gglrGeoLocationDetails :: Lens' GetGeoLocationResponse GeoLocationDetails
gglrGeoLocationDetails = lens _gglrGeoLocationDetails (\ s a -> s{_gglrGeoLocationDetails = a});
