{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.SNS.GetEndpointAttributes
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Retrieves the endpoint attributes for a device on one of the supported
-- push notification services, such as GCM and APNS. For more information,
-- see
-- <http://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html Using Amazon SNS Mobile Push Notifications>.
--
-- <http://docs.aws.amazon.com/sns/latest/api/API_GetEndpointAttributes.html>
module Network.AWS.SNS.GetEndpointAttributes
    (
    -- * Request
      GetEndpointAttributes
    -- ** Request constructor
    , getEndpointAttributes
    -- ** Request lenses
    , geaEndpointARN

    -- * Response
    , GetEndpointAttributesResponse
    -- ** Response constructor
    , getEndpointAttributesResponse
    -- ** Response lenses
    , gearAttributes
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SNS.Types

-- | /See:/ 'getEndpointAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'geaEndpointARN'
newtype GetEndpointAttributes = GetEndpointAttributes'{_geaEndpointARN :: Text} deriving (Eq, Read, Show)

-- | 'GetEndpointAttributes' smart constructor.
getEndpointAttributes :: Text -> GetEndpointAttributes
getEndpointAttributes pEndpointARN = GetEndpointAttributes'{_geaEndpointARN = pEndpointARN};

-- | EndpointArn for GetEndpointAttributes input.
geaEndpointARN :: Lens' GetEndpointAttributes Text
geaEndpointARN = lens _geaEndpointARN (\ s a -> s{_geaEndpointARN = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest GetEndpointAttributes where
        type Sv GetEndpointAttributes = SNS
        type Rs GetEndpointAttributes =
             GetEndpointAttributesResponse
        request = post
        response
          = receiveXMLWrapper "GetEndpointAttributesResult"
              (\ s h x ->
                 GetEndpointAttributesResponse' <$>
                   (x .@? "Attributes" .!@ mempty >>=
                      may (parseXMLMap "entry" "key" "value")))

instance ToHeaders GetEndpointAttributes where
        toHeaders = const mempty

instance ToPath GetEndpointAttributes where
        toPath = const "/"

instance ToQuery GetEndpointAttributes where
        toQuery GetEndpointAttributes'{..}
          = mconcat
              ["Action" =: ("GetEndpointAttributes" :: ByteString),
               "Version" =: ("2010-03-31" :: ByteString),
               "EndpointArn" =: _geaEndpointARN]

-- | /See:/ 'getEndpointAttributesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gearAttributes'
newtype GetEndpointAttributesResponse = GetEndpointAttributesResponse'{_gearAttributes :: Maybe (Map Text Text)} deriving (Eq, Read, Show)

-- | 'GetEndpointAttributesResponse' smart constructor.
getEndpointAttributesResponse :: GetEndpointAttributesResponse
getEndpointAttributesResponse = GetEndpointAttributesResponse'{_gearAttributes = Nothing};

-- | Attributes include the following:
--
-- -   @CustomUserData@ -- arbitrary user data to associate with the
--     endpoint. Amazon SNS does not use this data. The data must be in
--     UTF-8 format and less than 2KB.
-- -   @Enabled@ -- flag that enables\/disables delivery to the endpoint.
--     Amazon SNS will set this to false when a notification service
--     indicates to Amazon SNS that the endpoint is invalid. Users can set
--     it back to true, typically after updating Token.
-- -   @Token@ -- device token, also referred to as a registration id, for
--     an app and mobile device. This is returned from the notification
--     service when an app and mobile device are registered with the
--     notification service.
gearAttributes :: Lens' GetEndpointAttributesResponse (HashMap Text Text)
gearAttributes = lens _gearAttributes (\ s a -> s{_gearAttributes = a}) . _Default . _Map;
