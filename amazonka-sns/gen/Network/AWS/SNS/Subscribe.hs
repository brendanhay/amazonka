{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.Subscribe
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Prepares to subscribe an endpoint by sending the endpoint a confirmation
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
    , srqEndpoint
    , srqTopicARN
    , srqProtocol

    -- * Response
    , SubscribeResponse
    -- ** Response constructor
    , subscribeResponse
    -- ** Response lenses
    , srsSubscriptionARN
    , srsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SNS.Types

-- | Input for Subscribe action.
--
-- /See:/ 'subscribe' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'srqEndpoint'
--
-- * 'srqTopicARN'
--
-- * 'srqProtocol'
data Subscribe = Subscribe'
    { _srqEndpoint :: !(Maybe Endpoint)
    , _srqTopicARN :: !Text
    , _srqProtocol :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Subscribe' smart constructor.
subscribe :: Text -> Text -> Subscribe
subscribe pTopicARN_ pProtocol_ =
    Subscribe'
    { _srqEndpoint = Nothing
    , _srqTopicARN = pTopicARN_
    , _srqProtocol = pProtocol_
    }

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
srqEndpoint :: Lens' Subscribe (Maybe Endpoint)
srqEndpoint = lens _srqEndpoint (\ s a -> s{_srqEndpoint = a});

-- | The ARN of the topic you want to subscribe to.
srqTopicARN :: Lens' Subscribe Text
srqTopicARN = lens _srqTopicARN (\ s a -> s{_srqTopicARN = a});

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
srqProtocol :: Lens' Subscribe Text
srqProtocol = lens _srqProtocol (\ s a -> s{_srqProtocol = a});

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
               "Endpoint" =: _srqEndpoint,
               "TopicArn" =: _srqTopicARN,
               "Protocol" =: _srqProtocol]

-- | Response for Subscribe action.
--
-- /See:/ 'subscribeResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'srsSubscriptionARN'
--
-- * 'srsStatus'
data SubscribeResponse = SubscribeResponse'
    { _srsSubscriptionARN :: !(Maybe Text)
    , _srsStatus          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SubscribeResponse' smart constructor.
subscribeResponse :: Int -> SubscribeResponse
subscribeResponse pStatus_ =
    SubscribeResponse'
    { _srsSubscriptionARN = Nothing
    , _srsStatus = pStatus_
    }

-- | The ARN of the subscription, if the service was able to create a
-- subscription immediately (without requiring endpoint owner
-- confirmation).
srsSubscriptionARN :: Lens' SubscribeResponse (Maybe Text)
srsSubscriptionARN = lens _srsSubscriptionARN (\ s a -> s{_srsSubscriptionARN = a});

-- | FIXME: Undocumented member.
srsStatus :: Lens' SubscribeResponse Int
srsStatus = lens _srsStatus (\ s a -> s{_srsStatus = a});
