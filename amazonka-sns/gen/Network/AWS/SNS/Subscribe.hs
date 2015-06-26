{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.SNS.Subscribe
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

-- | Prepares to subscribe an endpoint by sending the endpoint a confirmation
-- message. To actually create a subscription, the endpoint owner must call
-- the @ConfirmSubscription@ action with the token from the confirmation
-- message. Confirmation tokens are valid for three days.
--
-- <http://docs.aws.amazon.com/sns/latest/api/API_Subscribe.html>
module Network.AWS.SNS.Subscribe
    (
    -- * Request
      Subscribe
    -- ** Request constructor
    , subscribe
    -- ** Request lenses
    , sEndpoint
    , sTopicARN
    , sProtocol

    -- * Response
    , SubscribeResponse
    -- ** Response constructor
    , subscribeResponse
    -- ** Response lenses
    , srSubscriptionARN
    , srStatusCode
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SNS.Types

-- | Input for Subscribe action.
--
-- /See:/ 'subscribe' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sEndpoint'
--
-- * 'sTopicARN'
--
-- * 'sProtocol'
data Subscribe = Subscribe'{_sEndpoint :: Maybe Endpoint, _sTopicARN :: Text, _sProtocol :: Text} deriving (Eq, Read, Show)

-- | 'Subscribe' smart constructor.
subscribe :: Text -> Text -> Subscribe
subscribe pTopicARN pProtocol = Subscribe'{_sEndpoint = Nothing, _sTopicARN = pTopicARN, _sProtocol = pProtocol};

-- | The endpoint that you want to receive notifications. Endpoints vary by
-- protocol:
--
-- -   For the @http@ protocol, the endpoint is an URL beginning with
--     \"http:\/\/\"
-- -   For the @https@ protocol, the endpoint is a URL beginning with
--     \"https:\/\/\"
-- -   For the @email@ protocol, the endpoint is an email address
-- -   For the @email-json@ protocol, the endpoint is an email address
-- -   For the @sms@ protocol, the endpoint is a phone number of an
--     SMS-enabled device
-- -   For the @sqs@ protocol, the endpoint is the ARN of an Amazon SQS
--     queue
-- -   For the @application@ protocol, the endpoint is the EndpointArn of a
--     mobile app and device.
sEndpoint :: Lens' Subscribe (Maybe Endpoint)
sEndpoint = lens _sEndpoint (\ s a -> s{_sEndpoint = a});

-- | The ARN of the topic you want to subscribe to.
sTopicARN :: Lens' Subscribe Text
sTopicARN = lens _sTopicARN (\ s a -> s{_sTopicARN = a});

-- | The protocol you want to use. Supported protocols include:
--
-- -   @http@ -- delivery of JSON-encoded message via HTTP POST
-- -   @https@ -- delivery of JSON-encoded message via HTTPS POST
-- -   @email@ -- delivery of message via SMTP
-- -   @email-json@ -- delivery of JSON-encoded message via SMTP
-- -   @sms@ -- delivery of message via SMS
-- -   @sqs@ -- delivery of JSON-encoded message to an Amazon SQS queue
-- -   @application@ -- delivery of JSON-encoded message to an EndpointArn
--     for a mobile app and device.
sProtocol :: Lens' Subscribe Text
sProtocol = lens _sProtocol (\ s a -> s{_sProtocol = a});

instance AWSRequest Subscribe where
        type Sv Subscribe = SNS
        type Rs Subscribe = SubscribeResponse
        request = post
        response
          = receiveXMLWrapper "SubscribeResult"
              (\ s h x ->
                 SubscribeResponse' <$>
                   (x .@? "SubscriptionArn") <*> (pure (fromEnum s)))

instance ToHeaders Subscribe where
        toHeaders = const mempty

instance ToPath Subscribe where
        toPath = const "/"

instance ToQuery Subscribe where
        toQuery Subscribe'{..}
          = mconcat
              ["Action" =: ("Subscribe" :: ByteString),
               "Version" =: ("2010-03-31" :: ByteString),
               "Endpoint" =: _sEndpoint, "TopicArn" =: _sTopicARN,
               "Protocol" =: _sProtocol]

-- | Response for Subscribe action.
--
-- /See:/ 'subscribeResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'srSubscriptionARN'
--
-- * 'srStatusCode'
data SubscribeResponse = SubscribeResponse'{_srSubscriptionARN :: Maybe Text, _srStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'SubscribeResponse' smart constructor.
subscribeResponse :: Int -> SubscribeResponse
subscribeResponse pStatusCode = SubscribeResponse'{_srSubscriptionARN = Nothing, _srStatusCode = pStatusCode};

-- | The ARN of the subscription, if the service was able to create a
-- subscription immediately (without requiring endpoint owner
-- confirmation).
srSubscriptionARN :: Lens' SubscribeResponse (Maybe Text)
srSubscriptionARN = lens _srSubscriptionARN (\ s a -> s{_srSubscriptionARN = a});

-- | FIXME: Undocumented member.
srStatusCode :: Lens' SubscribeResponse Int
srStatusCode = lens _srStatusCode (\ s a -> s{_srStatusCode = a});
