{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.SNS.SetPlatformApplicationAttributes
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

-- | Sets the attributes of the platform application object for the supported
-- push notification services, such as APNS and GCM. For more information,
-- see
-- <http://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html Using Amazon SNS Mobile Push Notifications>.
--
-- <http://docs.aws.amazon.com/sns/latest/api/API_SetPlatformApplicationAttributes.html>
module Network.AWS.SNS.SetPlatformApplicationAttributes
    (
    -- * Request
      SetPlatformApplicationAttributes
    -- ** Request constructor
    , setPlatformApplicationAttributes
    -- ** Request lenses
    , spaaPlatformApplicationARN
    , spaaAttributes

    -- * Response
    , SetPlatformApplicationAttributesResponse
    -- ** Response constructor
    , setPlatformApplicationAttributesResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SNS.Types

-- | Input for SetPlatformApplicationAttributes action.
--
-- /See:/ 'setPlatformApplicationAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'spaaPlatformApplicationARN'
--
-- * 'spaaAttributes'
data SetPlatformApplicationAttributes = SetPlatformApplicationAttributes'
    { _spaaPlatformApplicationARN :: Text
    , _spaaAttributes             :: Map Text Text
    } deriving (Eq,Read,Show)

-- | 'SetPlatformApplicationAttributes' smart constructor.
setPlatformApplicationAttributes :: Text -> SetPlatformApplicationAttributes
setPlatformApplicationAttributes pPlatformApplicationARN =
    SetPlatformApplicationAttributes'
    { _spaaPlatformApplicationARN = pPlatformApplicationARN
    , _spaaAttributes = mempty
    }

-- | PlatformApplicationArn for SetPlatformApplicationAttributes action.
spaaPlatformApplicationARN :: Lens' SetPlatformApplicationAttributes Text
spaaPlatformApplicationARN = lens _spaaPlatformApplicationARN (\ s a -> s{_spaaPlatformApplicationARN = a});

-- | A map of the platform application attributes. Attributes in this map
-- include the following:
--
-- -   @PlatformCredential@ -- The credential received from the
--     notification service. For APNS\/APNS_SANDBOX, PlatformCredential is
--     \"private key\". For GCM, PlatformCredential is \"API key\". For
--     ADM, PlatformCredential is \"client secret\".
-- -   @PlatformPrincipal@ -- The principal received from the notification
--     service. For APNS\/APNS_SANDBOX, PlatformPrincipal is \"SSL
--     certificate\". For GCM, PlatformPrincipal is not applicable. For
--     ADM, PlatformPrincipal is \"client id\".
-- -   @EventEndpointCreated@ -- Topic ARN to which EndpointCreated event
--     notifications should be sent.
-- -   @EventEndpointDeleted@ -- Topic ARN to which EndpointDeleted event
--     notifications should be sent.
-- -   @EventEndpointUpdated@ -- Topic ARN to which EndpointUpdate event
--     notifications should be sent.
-- -   @EventDeliveryFailure@ -- Topic ARN to which DeliveryFailure event
--     notifications should be sent upon Direct Publish delivery failure
--     (permanent) to one of the application\'s endpoints.
spaaAttributes :: Lens' SetPlatformApplicationAttributes (HashMap Text Text)
spaaAttributes = lens _spaaAttributes (\ s a -> s{_spaaAttributes = a}) . _Map;

instance AWSRequest SetPlatformApplicationAttributes
         where
        type Sv SetPlatformApplicationAttributes = SNS
        type Rs SetPlatformApplicationAttributes =
             SetPlatformApplicationAttributesResponse
        request = post
        response
          = receiveNull
              SetPlatformApplicationAttributesResponse'

instance ToHeaders SetPlatformApplicationAttributes
         where
        toHeaders = const mempty

instance ToPath SetPlatformApplicationAttributes
         where
        toPath = const "/"

instance ToQuery SetPlatformApplicationAttributes
         where
        toQuery SetPlatformApplicationAttributes'{..}
          = mconcat
              ["Action" =:
                 ("SetPlatformApplicationAttributes" :: ByteString),
               "Version" =: ("2010-03-31" :: ByteString),
               "PlatformApplicationArn" =:
                 _spaaPlatformApplicationARN,
               "Attributes" =:
                 toQueryMap "entry" "key" "value" _spaaAttributes]

-- | /See:/ 'setPlatformApplicationAttributesResponse' smart constructor.
data SetPlatformApplicationAttributesResponse =
    SetPlatformApplicationAttributesResponse'
    deriving (Eq,Read,Show)

-- | 'SetPlatformApplicationAttributesResponse' smart constructor.
setPlatformApplicationAttributesResponse :: SetPlatformApplicationAttributesResponse
setPlatformApplicationAttributesResponse =
    SetPlatformApplicationAttributesResponse'
