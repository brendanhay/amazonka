{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.GetGeoLocation
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- To retrieve a single geo location, send a 'GET' request to the
-- '\/Route 53 API version\/geolocation' resource with one of these
-- options: continentcode | countrycode | countrycode and subdivisioncode.
module Network.AWS.Route53.GetGeoLocation
    (
    -- * Creating a Request
      getGeoLocation
    , GetGeoLocation
    -- * Request Lenses
    , gglSubdivisionCode
    , gglCountryCode
    , gglContinentCode

    -- * Destructuring the Response
    , getGeoLocationResponse
    , GetGeoLocationResponse
    -- * Response Lenses
    , gglrsResponseStatus
    , gglrsGeoLocationDetails
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.Route53.Types
import           Network.AWS.Route53.Types.Product

-- | A complex type that contains information about the request to get a geo
-- location.
--
-- /See:/ 'getGeoLocation' smart constructor.
data GetGeoLocation = GetGeoLocation'
    { _gglSubdivisionCode :: !(Maybe Text)
    , _gglCountryCode     :: !(Maybe Text)
    , _gglContinentCode   :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetGeoLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gglSubdivisionCode'
--
-- * 'gglCountryCode'
--
-- * 'gglContinentCode'
getGeoLocation
    :: GetGeoLocation
getGeoLocation =
    GetGeoLocation'
    { _gglSubdivisionCode = Nothing
    , _gglCountryCode = Nothing
    , _gglContinentCode = Nothing
    }

-- | The code for a country\'s subdivision (e.g., a province of Canada). A
-- subdivision code is only valid with the appropriate country code.
--
-- Constraint: Specifying 'SubdivisionCode' without 'CountryCode' returns
-- an < InvalidInput> error.
gglSubdivisionCode :: Lens' GetGeoLocation (Maybe Text)
gglSubdivisionCode = lens _gglSubdivisionCode (\ s a -> s{_gglSubdivisionCode = a});

-- | The code for a country geo location. The default location uses \'*\' for
-- the country code and will match all locations that are not matched by a
-- geo location.
--
-- The default geo location uses a '*' for the country code. All other
-- country codes follow the ISO 3166 two-character code.
gglCountryCode :: Lens' GetGeoLocation (Maybe Text)
gglCountryCode = lens _gglCountryCode (\ s a -> s{_gglCountryCode = a});

-- | The code for a continent geo location. Note: only continent locations
-- have a continent code.
--
-- Valid values: 'AF' | 'AN' | 'AS' | 'EU' | 'OC' | 'NA' | 'SA'
--
-- Constraint: Specifying 'ContinentCode' with either 'CountryCode' or
-- 'SubdivisionCode' returns an < InvalidInput> error.
gglContinentCode :: Lens' GetGeoLocation (Maybe Text)
gglContinentCode = lens _gglContinentCode (\ s a -> s{_gglContinentCode = a});

instance AWSRequest GetGeoLocation where
        type Rs GetGeoLocation = GetGeoLocationResponse
        request = get route53
        response
          = receiveXML
              (\ s h x ->
                 GetGeoLocationResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "GeoLocationDetails"))

instance Hashable GetGeoLocation

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
data GetGeoLocationResponse = GetGeoLocationResponse'
    { _gglrsResponseStatus     :: !Int
    , _gglrsGeoLocationDetails :: !GeoLocationDetails
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetGeoLocationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gglrsResponseStatus'
--
-- * 'gglrsGeoLocationDetails'
getGeoLocationResponse
    :: Int -- ^ 'gglrsResponseStatus'
    -> GeoLocationDetails -- ^ 'gglrsGeoLocationDetails'
    -> GetGeoLocationResponse
getGeoLocationResponse pResponseStatus_ pGeoLocationDetails_ =
    GetGeoLocationResponse'
    { _gglrsResponseStatus = pResponseStatus_
    , _gglrsGeoLocationDetails = pGeoLocationDetails_
    }

-- | The response status code.
gglrsResponseStatus :: Lens' GetGeoLocationResponse Int
gglrsResponseStatus = lens _gglrsResponseStatus (\ s a -> s{_gglrsResponseStatus = a});

-- | A complex type that contains the information about the specified geo
-- location.
gglrsGeoLocationDetails :: Lens' GetGeoLocationResponse GeoLocationDetails
gglrsGeoLocationDetails = lens _gglrsGeoLocationDetails (\ s a -> s{_gglrsGeoLocationDetails = a});
