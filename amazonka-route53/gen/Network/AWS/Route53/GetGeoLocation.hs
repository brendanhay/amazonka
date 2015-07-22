{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetGeoLocation
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- To retrieve a single geo location, send a @GET@ request to the
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
    , gglrqSubdivisionCode
    , gglrqCountryCode
    , gglrqContinentCode

    -- * Response
    , GetGeoLocationResponse
    -- ** Response constructor
    , getGeoLocationResponse
    -- ** Response lenses
    , gglrsStatus
    , gglrsGeoLocationDetails
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
-- * 'gglrqSubdivisionCode'
--
-- * 'gglrqCountryCode'
--
-- * 'gglrqContinentCode'
data GetGeoLocation = GetGeoLocation'
    { _gglrqSubdivisionCode :: !(Maybe Text)
    , _gglrqCountryCode     :: !(Maybe Text)
    , _gglrqContinentCode   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetGeoLocation' smart constructor.
getGeoLocation :: GetGeoLocation
getGeoLocation =
    GetGeoLocation'
    { _gglrqSubdivisionCode = Nothing
    , _gglrqCountryCode = Nothing
    , _gglrqContinentCode = Nothing
    }

-- | The code for a country\'s subdivision (e.g., a province of Canada). A
-- subdivision code is only valid with the appropriate country code.
--
-- Constraint: Specifying @SubdivisionCode@ without @CountryCode@ returns
-- an InvalidInput error.
gglrqSubdivisionCode :: Lens' GetGeoLocation (Maybe Text)
gglrqSubdivisionCode = lens _gglrqSubdivisionCode (\ s a -> s{_gglrqSubdivisionCode = a});

-- | The code for a country geo location. The default location uses \'*\' for
-- the country code and will match all locations that are not matched by a
-- geo location.
--
-- The default geo location uses a @*@ for the country code. All other
-- country codes follow the ISO 3166 two-character code.
gglrqCountryCode :: Lens' GetGeoLocation (Maybe Text)
gglrqCountryCode = lens _gglrqCountryCode (\ s a -> s{_gglrqCountryCode = a});

-- | The code for a continent geo location. Note: only continent locations
-- have a continent code.
--
-- Valid values: @AF@ | @AN@ | @AS@ | @EU@ | @OC@ | @NA@ | @SA@
--
-- Constraint: Specifying @ContinentCode@ with either @CountryCode@ or
-- @SubdivisionCode@ returns an InvalidInput error.
gglrqContinentCode :: Lens' GetGeoLocation (Maybe Text)
gglrqContinentCode = lens _gglrqContinentCode (\ s a -> s{_gglrqContinentCode = a});

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
              ["subdivisioncode" =: _gglrqSubdivisionCode,
               "countrycode" =: _gglrqCountryCode,
               "continentcode" =: _gglrqContinentCode]

-- | A complex type containing information about the specified geo location.
--
-- /See:/ 'getGeoLocationResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gglrsStatus'
--
-- * 'gglrsGeoLocationDetails'
data GetGeoLocationResponse = GetGeoLocationResponse'
    { _gglrsStatus             :: !Int
    , _gglrsGeoLocationDetails :: !GeoLocationDetails
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetGeoLocationResponse' smart constructor.
getGeoLocationResponse :: Int -> GeoLocationDetails -> GetGeoLocationResponse
getGeoLocationResponse pStatus pGeoLocationDetails =
    GetGeoLocationResponse'
    { _gglrsStatus = pStatus
    , _gglrsGeoLocationDetails = pGeoLocationDetails
    }

-- | FIXME: Undocumented member.
gglrsStatus :: Lens' GetGeoLocationResponse Int
gglrsStatus = lens _gglrsStatus (\ s a -> s{_gglrsStatus = a});

-- | A complex type that contains the information about the specified geo
-- location.
gglrsGeoLocationDetails :: Lens' GetGeoLocationResponse GeoLocationDetails
gglrsGeoLocationDetails = lens _gglrsGeoLocationDetails (\ s a -> s{_gglrsGeoLocationDetails = a});
