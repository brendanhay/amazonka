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
-- Module      : Network.AWS.SNS.SetPlatformApplicationAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the attributes of the platform application object for the supported push notification services, such as APNS and GCM. For more information, see <http://docs.aws.amazon.com/sns/latest/dg/SNSMobilePush.html Using Amazon SNS Mobile Push Notifications> . For information on configuring attributes for message delivery status, see <http://docs.aws.amazon.com/sns/latest/dg/sns-msg-status.html Using Amazon SNS Application Attributes for Message Delivery Status> .
--
--
module Network.AWS.SNS.SetPlatformApplicationAttributes
    (
    -- * Creating a Request
      setPlatformApplicationAttributes
    , SetPlatformApplicationAttributes
    -- * Request Lenses
    , spaaPlatformApplicationARN
    , spaaAttributes

    -- * Destructuring the Response
    , setPlatformApplicationAttributesResponse
    , SetPlatformApplicationAttributesResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SNS.Types
import Network.AWS.SNS.Types.Product

-- | Input for SetPlatformApplicationAttributes action.
--
--
--
-- /See:/ 'setPlatformApplicationAttributes' smart constructor.
data SetPlatformApplicationAttributes = SetPlatformApplicationAttributes'
  { _spaaPlatformApplicationARN :: !Text
  , _spaaAttributes             :: !(Map Text Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetPlatformApplicationAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spaaPlatformApplicationARN' - PlatformApplicationArn for SetPlatformApplicationAttributes action.
--
-- * 'spaaAttributes' - A map of the platform application attributes. Attributes in this map include the following:     * @PlatformCredential@ -- The credential received from the notification service. For APNS/APNS_SANDBOX, PlatformCredential is private key. For GCM, PlatformCredential is "API key". For ADM, PlatformCredential is "client secret".     * @PlatformPrincipal@ -- The principal received from the notification service. For APNS/APNS_SANDBOX, PlatformPrincipal is SSL certificate. For GCM, PlatformPrincipal is not applicable. For ADM, PlatformPrincipal is "client id".     * @EventEndpointCreated@ -- Topic ARN to which EndpointCreated event notifications should be sent.     * @EventEndpointDeleted@ -- Topic ARN to which EndpointDeleted event notifications should be sent.     * @EventEndpointUpdated@ -- Topic ARN to which EndpointUpdate event notifications should be sent.     * @EventDeliveryFailure@ -- Topic ARN to which DeliveryFailure event notifications should be sent upon Direct Publish delivery failure (permanent) to one of the application's endpoints.     * @SuccessFeedbackRoleArn@ -- IAM role ARN used to give Amazon SNS write access to use CloudWatch Logs on your behalf.     * @FailureFeedbackRoleArn@ -- IAM role ARN used to give Amazon SNS write access to use CloudWatch Logs on your behalf.     * @SuccessFeedbackSampleRate@ -- Sample rate percentage (0-100) of successfully delivered messages.
setPlatformApplicationAttributes
    :: Text -- ^ 'spaaPlatformApplicationARN'
    -> SetPlatformApplicationAttributes
setPlatformApplicationAttributes pPlatformApplicationARN_ =
  SetPlatformApplicationAttributes'
    { _spaaPlatformApplicationARN = pPlatformApplicationARN_
    , _spaaAttributes = mempty
    }


-- | PlatformApplicationArn for SetPlatformApplicationAttributes action.
spaaPlatformApplicationARN :: Lens' SetPlatformApplicationAttributes Text
spaaPlatformApplicationARN = lens _spaaPlatformApplicationARN (\ s a -> s{_spaaPlatformApplicationARN = a})

-- | A map of the platform application attributes. Attributes in this map include the following:     * @PlatformCredential@ -- The credential received from the notification service. For APNS/APNS_SANDBOX, PlatformCredential is private key. For GCM, PlatformCredential is "API key". For ADM, PlatformCredential is "client secret".     * @PlatformPrincipal@ -- The principal received from the notification service. For APNS/APNS_SANDBOX, PlatformPrincipal is SSL certificate. For GCM, PlatformPrincipal is not applicable. For ADM, PlatformPrincipal is "client id".     * @EventEndpointCreated@ -- Topic ARN to which EndpointCreated event notifications should be sent.     * @EventEndpointDeleted@ -- Topic ARN to which EndpointDeleted event notifications should be sent.     * @EventEndpointUpdated@ -- Topic ARN to which EndpointUpdate event notifications should be sent.     * @EventDeliveryFailure@ -- Topic ARN to which DeliveryFailure event notifications should be sent upon Direct Publish delivery failure (permanent) to one of the application's endpoints.     * @SuccessFeedbackRoleArn@ -- IAM role ARN used to give Amazon SNS write access to use CloudWatch Logs on your behalf.     * @FailureFeedbackRoleArn@ -- IAM role ARN used to give Amazon SNS write access to use CloudWatch Logs on your behalf.     * @SuccessFeedbackSampleRate@ -- Sample rate percentage (0-100) of successfully delivered messages.
spaaAttributes :: Lens' SetPlatformApplicationAttributes (HashMap Text Text)
spaaAttributes = lens _spaaAttributes (\ s a -> s{_spaaAttributes = a}) . _Map

instance AWSRequest SetPlatformApplicationAttributes
         where
        type Rs SetPlatformApplicationAttributes =
             SetPlatformApplicationAttributesResponse
        request = postQuery sns
        response
          = receiveNull
              SetPlatformApplicationAttributesResponse'

instance Hashable SetPlatformApplicationAttributes
         where

instance NFData SetPlatformApplicationAttributes
         where

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
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetPlatformApplicationAttributesResponse' with the minimum fields required to make a request.
--
setPlatformApplicationAttributesResponse
    :: SetPlatformApplicationAttributesResponse
setPlatformApplicationAttributesResponse =
  SetPlatformApplicationAttributesResponse'


instance NFData
           SetPlatformApplicationAttributesResponse
         where
