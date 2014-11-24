{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
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
    , sProtocol
    , sTopicArn

    -- * Response
    , SubscribeResponse
    -- ** Response constructor
    , subscribeResponse
    -- ** Response lenses
    , srSubscriptionArn
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SNS.Types
import qualified GHC.Exts

data Subscribe = Subscribe
    { _sEndpoint :: Maybe Text
    , _sProtocol :: Text
    , _sTopicArn :: Text
    } deriving (Eq, Ord, Show)

-- | 'Subscribe' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sEndpoint' @::@ 'Maybe' 'Text'
--
-- * 'sProtocol' @::@ 'Text'
--
-- * 'sTopicArn' @::@ 'Text'
--
subscribe :: Text -- ^ 'sTopicArn'
          -> Text -- ^ 'sProtocol'
          -> Subscribe
subscribe p1 p2 = Subscribe
    { _sTopicArn = p1
    , _sProtocol = p2
    , _sEndpoint = Nothing
    }

-- | The endpoint that you want to receive notifications. Endpoints vary by
-- protocol: For the @http@ protocol, the endpoint is an URL beginning with
-- "http://" For the @https@ protocol, the endpoint is a URL beginning with
-- "https://" For the @email@ protocol, the endpoint is an email address For
-- the @email-json@ protocol, the endpoint is an email address For the @sms@
-- protocol, the endpoint is a phone number of an SMS-enabled device For the
-- @sqs@ protocol, the endpoint is the ARN of an Amazon SQS queue For the
-- @application@ protocol, the endpoint is the EndpointArn of a mobile app
-- and device.
sEndpoint :: Lens' Subscribe (Maybe Text)
sEndpoint = lens _sEndpoint (\s a -> s { _sEndpoint = a })

-- | The protocol you want to use. Supported protocols include: @http@ --
-- delivery of JSON-encoded message via HTTP POST @https@ -- delivery of
-- JSON-encoded message via HTTPS POST @email@ -- delivery of message via
-- SMTP @email-json@ -- delivery of JSON-encoded message via SMTP @sms@ --
-- delivery of message via SMS @sqs@ -- delivery of JSON-encoded message to
-- an Amazon SQS queue @application@ -- delivery of JSON-encoded message to
-- an EndpointArn for a mobile app and device.
sProtocol :: Lens' Subscribe Text
sProtocol = lens _sProtocol (\s a -> s { _sProtocol = a })

-- | The ARN of the topic you want to subscribe to.
sTopicArn :: Lens' Subscribe Text
sTopicArn = lens _sTopicArn (\s a -> s { _sTopicArn = a })

newtype SubscribeResponse = SubscribeResponse
    { _srSubscriptionArn :: Maybe Text
    } deriving (Eq, Ord, Show, Monoid)

-- | 'SubscribeResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'srSubscriptionArn' @::@ 'Maybe' 'Text'
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

instance ToPath Subscribe where
    toPath = const "/"

instance ToQuery Subscribe where
    toQuery Subscribe{..} = mconcat
        [ "Endpoint" =? _sEndpoint
        , "Protocol" =? _sProtocol
        , "TopicArn" =? _sTopicArn
        ]

instance ToHeaders Subscribe

instance AWSRequest Subscribe where
    type Sv Subscribe = SNS
    type Rs Subscribe = SubscribeResponse

    request  = post "Subscribe"
    response = xmlResponse

instance FromXML SubscribeResponse where
    parseXML = withElement "SubscribeResult" $ \x -> SubscribeResponse
        <$> x .@? "SubscriptionArn"
