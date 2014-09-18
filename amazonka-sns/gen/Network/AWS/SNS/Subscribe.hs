{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.Subscribe
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Prepares to subscribe an endpoint by sending the endpoint a confirmation
-- message. To actually create a subscription, the endpoint owner must call
-- the ConfirmSubscription action with the token from the confirmation
-- message. Confirmation tokens are valid for three days. The following
-- example Query request subscribes an SQS queue to an SNS topic. For more
-- information, see Subscribe Queue to Amazon SNS Topic in the Amazon SQS
-- Developer Guide. http://sns.us-west-2.amazonaws.com/ &amp;Action=Subscribe
-- &amp;Endpoint=arn%3Aaws%3Asqs%3Aus-west-2%3A123456789012%3AMyQueue
-- &amp;Version=2010-03-31 &amp;Protocol=sqs
-- &amp;TopicArn=arn%3Aaws%3Asns%3Aus-west-2%3A123456789012%3AMyTopic
-- &lt;SubscribeResponse xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;SubscribeResult&gt;
-- &lt;SubscriptionArn&gt;arn:aws:sns:us-west-2:123456789012:MyTopic:6b0e71bd-7e97-4d97-80ce-4a0994e55286&lt;/SubscriptionArn&gt;
-- &lt;/SubscribeResult&gt; &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;c4407779-24a4-56fa-982c-3d927f93a775&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/SubscribeResponse&gt;.
module Network.AWS.SNS.Subscribe
    (
    -- * Request
      Subscribe
    -- ** Request constructor
    , subscribe
    -- ** Request lenses
    , s1TopicArn
    , s1Protocol
    , s1Endpoint

    -- * Response
    , SubscribeResponse
    -- ** Response constructor
    , subscribeResponse
    -- ** Response lenses
    , srSubscriptionArn
    ) where

import Network.AWS.Request.Query
import Network.AWS.SNS.Types
import Network.AWS.Prelude

-- | Input for Subscribe action.
data Subscribe = Subscribe
    { _s1TopicArn :: Text
    , _s1Protocol :: Text
    , _s1Endpoint :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'Subscribe' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @TopicArn ::@ @Text@
--
-- * @Protocol ::@ @Text@
--
-- * @Endpoint ::@ @Maybe Text@
--
subscribe :: Text -- ^ 's1TopicArn'
            -> Text -- ^ 's1Protocol'
            -> Subscribe
subscribe p1 p2 = Subscribe
    { _s1TopicArn = p1
    , _s1Protocol = p2
    , _s1Endpoint = Nothing
    }

-- | The ARN of the topic you want to subscribe to.
s1TopicArn :: Lens' Subscribe Text
s1TopicArn = lens _s1TopicArn (\s a -> s { _s1TopicArn = a })

-- | The protocol you want to use. Supported protocols include: http -- delivery
-- of JSON-encoded message via HTTP POST https -- delivery of JSON-encoded
-- message via HTTPS POST email -- delivery of message via SMTP email-json --
-- delivery of JSON-encoded message via SMTP sms -- delivery of message via
-- SMS sqs -- delivery of JSON-encoded message to an Amazon SQS queue
-- application -- delivery of JSON-encoded message to an EndpointArn for a
-- mobile app and device.
s1Protocol :: Lens' Subscribe Text
s1Protocol = lens _s1Protocol (\s a -> s { _s1Protocol = a })

-- | The endpoint that you want to receive notifications. Endpoints vary by
-- protocol: For the http protocol, the endpoint is an URL beginning with
-- "http://" For the https protocol, the endpoint is a URL beginning with
-- "https://" For the email protocol, the endpoint is an email address For the
-- email-json protocol, the endpoint is an email address For the sms protocol,
-- the endpoint is a phone number of an SMS-enabled device For the sqs
-- protocol, the endpoint is the ARN of an Amazon SQS queue For the
-- application protocol, the endpoint is the EndpointArn of a mobile app and
-- device.
s1Endpoint :: Lens' Subscribe (Maybe Text)
s1Endpoint = lens _s1Endpoint (\s a -> s { _s1Endpoint = a })

instance ToQuery Subscribe where
    toQuery = genericQuery def

-- | Response for Subscribe action.
newtype SubscribeResponse = SubscribeResponse
    { _srSubscriptionArn :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SubscribeResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SubscriptionArn ::@ @Maybe Text@
--
subscribeResponse :: SubscribeResponse
subscribeResponse = SubscribeResponse
    { _srSubscriptionArn = Nothing
    }

-- | The ARN of the subscription, if the service was able to create a
-- subscription immediately (without requiring endpoint owner confirmation).
srSubscriptionArn :: Lens' SubscribeResponse (Maybe Text)
srSubscriptionArn =
    lens _srSubscriptionArn (\s a -> s { _srSubscriptionArn = a })

instance FromXML SubscribeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest Subscribe where
    type Sv Subscribe = SNS
    type Rs Subscribe = SubscribeResponse

    request = post "Subscribe"
    response _ = xmlResponse
