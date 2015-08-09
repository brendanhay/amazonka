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
-- Module      : Network.AWS.SNS.Subscribe
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Prepares to subscribe an endpoint by sending the endpoint a confirmation
-- message. To actually create a subscription, the endpoint owner must call
-- the @ConfirmSubscription@ action with the token from the confirmation
-- message. Confirmation tokens are valid for three days.
--
-- /See:/ <http://docs.aws.amazon.com/sns/latest/api/API_Subscribe.html AWS API Reference> for Subscribe.
module Network.AWS.SNS.Subscribe
    (
    -- * Creating a Request
      Subscribe
    , subscribe
    -- * Request Lenses
    , subEndpoint
    , subTopicARN
    , subProtocol

    -- * Destructuring the Response
    , SubscribeResponse
    , subscribeResponse
    -- * Response Lenses
    , srsSubscriptionARN
    , srsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SNS.Types
import           Network.AWS.SNS.Types.Product

-- | Input for Subscribe action.
--
-- /See:/ 'subscribe' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'subEndpoint'
--
-- * 'subTopicARN'
--
-- * 'subProtocol'
data Subscribe = Subscribe'
    { _subEndpoint :: !(Maybe Endpoint)
    , _subTopicARN :: !Text
    , _subProtocol :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'Subscribe' smart constructor.
subscribe :: Text -> Text -> Subscribe
subscribe pTopicARN_ pProtocol_ =
    Subscribe'
    { _subEndpoint = Nothing
    , _subTopicARN = pTopicARN_
    , _subProtocol = pProtocol_
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
subEndpoint :: Lens' Subscribe (Maybe Endpoint)
subEndpoint = lens _subEndpoint (\ s a -> s{_subEndpoint = a});

-- | The ARN of the topic you want to subscribe to.
subTopicARN :: Lens' Subscribe Text
subTopicARN = lens _subTopicARN (\ s a -> s{_subTopicARN = a});

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
subProtocol :: Lens' Subscribe Text
subProtocol = lens _subProtocol (\ s a -> s{_subProtocol = a});

instance AWSRequest Subscribe where
        type Sv Subscribe = SNS
        type Rs Subscribe = SubscribeResponse
        request = postQuery
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
               "Endpoint" =: _subEndpoint,
               "TopicArn" =: _subTopicARN,
               "Protocol" =: _subProtocol]

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

-- | Undocumented member.
srsStatus :: Lens' SubscribeResponse Int
srsStatus = lens _srsStatus (\ s a -> s{_srsStatus = a});
