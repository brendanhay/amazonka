{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.SNS.GetPlatformApplicationAttributes
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

-- | Retrieves the attributes of the platform application object for the
-- supported push notification services, such as APNS and GCM. For more
-- information, see
-- <http://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html Using Amazon SNS Mobile Push Notifications>.
--
-- <http://docs.aws.amazon.com/sns/latest/api/API_GetPlatformApplicationAttributes.html>
module Network.AWS.SNS.GetPlatformApplicationAttributes
    (
    -- * Request
      GetPlatformApplicationAttributes
    -- ** Request constructor
    , getPlatformApplicationAttributes
    -- ** Request lenses
    , gpaaPlatformApplicationARN

    -- * Response
    , GetPlatformApplicationAttributesResponse
    -- ** Response constructor
    , getPlatformApplicationAttributesResponse
    -- ** Response lenses
    , gpaarAttributes
    , gpaarStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SNS.Types

-- | Input for GetPlatformApplicationAttributes action.
--
-- /See:/ 'getPlatformApplicationAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gpaaPlatformApplicationARN'
newtype GetPlatformApplicationAttributes = GetPlatformApplicationAttributes'
    { _gpaaPlatformApplicationARN :: Text
    } deriving (Eq,Read,Show)

-- | 'GetPlatformApplicationAttributes' smart constructor.
getPlatformApplicationAttributes :: Text -> GetPlatformApplicationAttributes
getPlatformApplicationAttributes pPlatformApplicationARN =
    GetPlatformApplicationAttributes'
    { _gpaaPlatformApplicationARN = pPlatformApplicationARN
    }

-- | PlatformApplicationArn for GetPlatformApplicationAttributesInput.
gpaaPlatformApplicationARN :: Lens' GetPlatformApplicationAttributes Text
gpaaPlatformApplicationARN = lens _gpaaPlatformApplicationARN (\ s a -> s{_gpaaPlatformApplicationARN = a});

instance AWSRequest GetPlatformApplicationAttributes
         where
        type Sv GetPlatformApplicationAttributes = SNS
        type Rs GetPlatformApplicationAttributes =
             GetPlatformApplicationAttributesResponse
        request = post
        response
          = receiveXMLWrapper
              "GetPlatformApplicationAttributesResult"
              (\ s h x ->
                 GetPlatformApplicationAttributesResponse' <$>
                   (x .@? "Attributes" .!@ mempty >>=
                      may (parseXMLMap "entry" "key" "value"))
                     <*> (pure s))

instance ToHeaders GetPlatformApplicationAttributes
         where
        toHeaders = const mempty

instance ToPath GetPlatformApplicationAttributes
         where
        toPath = const "/"

instance ToQuery GetPlatformApplicationAttributes
         where
        toQuery GetPlatformApplicationAttributes'{..}
          = mconcat
              ["Action" =:
                 ("GetPlatformApplicationAttributes" :: ByteString),
               "Version" =: ("2010-03-31" :: ByteString),
               "PlatformApplicationArn" =:
                 _gpaaPlatformApplicationARN]

-- | Response for GetPlatformApplicationAttributes action.
--
-- /See:/ 'getPlatformApplicationAttributesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gpaarAttributes'
--
-- * 'gpaarStatus'
data GetPlatformApplicationAttributesResponse = GetPlatformApplicationAttributesResponse'
    { _gpaarAttributes :: !(Maybe (Map Text Text))
    , _gpaarStatus     :: !Status
    } deriving (Eq,Read,Show)

-- | 'GetPlatformApplicationAttributesResponse' smart constructor.
getPlatformApplicationAttributesResponse :: Status -> GetPlatformApplicationAttributesResponse
getPlatformApplicationAttributesResponse pStatus =
    GetPlatformApplicationAttributesResponse'
    { _gpaarAttributes = Nothing
    , _gpaarStatus = pStatus
    }

-- | Attributes include the following:
--
-- -   @EventEndpointCreated@ -- Topic ARN to which EndpointCreated event
--     notifications should be sent.
-- -   @EventEndpointDeleted@ -- Topic ARN to which EndpointDeleted event
--     notifications should be sent.
-- -   @EventEndpointUpdated@ -- Topic ARN to which EndpointUpdate event
--     notifications should be sent.
-- -   @EventDeliveryFailure@ -- Topic ARN to which DeliveryFailure event
--     notifications should be sent upon Direct Publish delivery failure
--     (permanent) to one of the application\'s endpoints.
gpaarAttributes :: Lens' GetPlatformApplicationAttributesResponse (HashMap Text Text)
gpaarAttributes = lens _gpaarAttributes (\ s a -> s{_gpaarAttributes = a}) . _Default . _Map;

-- | FIXME: Undocumented member.
gpaarStatus :: Lens' GetPlatformApplicationAttributesResponse Status
gpaarStatus = lens _gpaarStatus (\ s a -> s{_gpaarStatus = a});
