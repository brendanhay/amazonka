{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.V2010_03_31.Subscribe
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
module Network.AWS.SNS.V2010_03_31.Subscribe
    (
    -- * Request
      Subscribe
    -- ** Request constructor
    , subscribe
    -- ** Request lenses
    , ssyProtocol
    , ssyTopicArn
    , ssyEndpoint

    -- * Response
    , SubscribeResponse
    -- ** Response lenses
    , sseSubscriptionArn
    ) where

import Network.AWS.Request.Query
import Network.AWS.SNS.V2010_03_31.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'Subscribe' request.
subscribe :: Text -- ^ 'ssyProtocol'
          -> Text -- ^ 'ssyTopicArn'
          -> Subscribe
subscribe p1 p2 = Subscribe
    { _ssyProtocol = p1
    , _ssyTopicArn = p2
    , _ssyEndpoint = Nothing
    }

data Subscribe = Subscribe
    { _ssyProtocol :: Text
      -- ^ The protocol you want to use. Supported protocols include: http
      -- -- delivery of JSON-encoded message via HTTP POST https --
      -- delivery of JSON-encoded message via HTTPS POST email -- delivery
      -- of message via SMTP email-json -- delivery of JSON-encoded
      -- message via SMTP sms -- delivery of message via SMS sqs --
      -- delivery of JSON-encoded message to an Amazon SQS queue
      -- application -- delivery of JSON-encoded message to an EndpointArn
      -- for a mobile app and device.
    , _ssyTopicArn :: Text
      -- ^ The ARN of the topic you want to subscribe to.
    , _ssyEndpoint :: Maybe Text
      -- ^ The endpoint that you want to receive notifications. Endpoints
      -- vary by protocol: For the http protocol, the endpoint is an URL
      -- beginning with "http://" For the https protocol, the endpoint is
      -- a URL beginning with "https://" For the email protocol, the
      -- endpoint is an email address For the email-json protocol, the
      -- endpoint is an email address For the sms protocol, the endpoint
      -- is a phone number of an SMS-enabled device For the sqs protocol,
      -- the endpoint is the ARN of an Amazon SQS queue For the
      -- application protocol, the endpoint is the EndpointArn of a mobile
      -- app and device.
    } deriving (Show, Generic)

-- | The protocol you want to use. Supported protocols include: http -- delivery
-- of JSON-encoded message via HTTP POST https -- delivery of JSON-encoded
-- message via HTTPS POST email -- delivery of message via SMTP email-json --
-- delivery of JSON-encoded message via SMTP sms -- delivery of message via
-- SMS sqs -- delivery of JSON-encoded message to an Amazon SQS queue
-- application -- delivery of JSON-encoded message to an EndpointArn for a
-- mobile app and device.
ssyProtocol
    :: Functor f
    => (Text
    -> f (Text))
    -> Subscribe
    -> f Subscribe
ssyProtocol f x =
    (\y -> x { _ssyProtocol = y })
       <$> f (_ssyProtocol x)
{-# INLINE ssyProtocol #-}

-- | The ARN of the topic you want to subscribe to.
ssyTopicArn
    :: Functor f
    => (Text
    -> f (Text))
    -> Subscribe
    -> f Subscribe
ssyTopicArn f x =
    (\y -> x { _ssyTopicArn = y })
       <$> f (_ssyTopicArn x)
{-# INLINE ssyTopicArn #-}

-- | The endpoint that you want to receive notifications. Endpoints vary by
-- protocol: For the http protocol, the endpoint is an URL beginning with
-- "http://" For the https protocol, the endpoint is a URL beginning with
-- "https://" For the email protocol, the endpoint is an email address For the
-- email-json protocol, the endpoint is an email address For the sms protocol,
-- the endpoint is a phone number of an SMS-enabled device For the sqs
-- protocol, the endpoint is the ARN of an Amazon SQS queue For the
-- application protocol, the endpoint is the EndpointArn of a mobile app and
-- device.
ssyEndpoint
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> Subscribe
    -> f Subscribe
ssyEndpoint f x =
    (\y -> x { _ssyEndpoint = y })
       <$> f (_ssyEndpoint x)
{-# INLINE ssyEndpoint #-}

instance ToQuery Subscribe where
    toQuery = genericQuery def

data SubscribeResponse = SubscribeResponse
    { _sseSubscriptionArn :: Maybe Text
      -- ^ The ARN of the subscription, if the service was able to create a
      -- subscription immediately (without requiring endpoint owner
      -- confirmation).
    } deriving (Show, Generic)

-- | The ARN of the subscription, if the service was able to create a
-- subscription immediately (without requiring endpoint owner confirmation).
sseSubscriptionArn
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> SubscribeResponse
    -> f SubscribeResponse
sseSubscriptionArn f x =
    (\y -> x { _sseSubscriptionArn = y })
       <$> f (_sseSubscriptionArn x)
{-# INLINE sseSubscriptionArn #-}

instance FromXML SubscribeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest Subscribe where
    type Sv Subscribe = SNS
    type Rs Subscribe = SubscribeResponse

    request = post "Subscribe"
    response _ = xmlResponse
