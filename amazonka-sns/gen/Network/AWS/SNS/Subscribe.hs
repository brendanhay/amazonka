{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
-- message. Confirmation tokens are valid for three days.
module Network.AWS.SNS.Subscribe
    (
    -- * Request
      SubscribeInput
    -- ** Request constructor
    , subscribeInput
    -- ** Request lenses
    , siEndpoint
    , siProtocol
    , siTopicArn

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

data SubscribeInput = SubscribeInput
    { _siEndpoint :: Maybe Text
    , _siProtocol :: Text
    , _siTopicArn :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'SubscribeInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'siEndpoint' @::@ 'Maybe' 'Text'
--
-- * 'siProtocol' @::@ 'Text'
--
-- * 'siTopicArn' @::@ 'Text'
--
subscribeInput :: Text -- ^ 'siTopicArn'
               -> Text -- ^ 'siProtocol'
               -> SubscribeInput
subscribeInput p1 p2 = SubscribeInput
    { _siTopicArn = p1
    , _siProtocol = p2
    , _siEndpoint = Nothing
    }

-- | The endpoint that you want to receive notifications. Endpoints vary by
-- protocol: For the http protocol, the endpoint is an URL beginning with
-- "http://" For the https protocol, the endpoint is a URL beginning with
-- "https://" For the email protocol, the endpoint is an email address For
-- the email-json protocol, the endpoint is an email address For the sms
-- protocol, the endpoint is a phone number of an SMS-enabled device For the
-- sqs protocol, the endpoint is the ARN of an Amazon SQS queue For the
-- application protocol, the endpoint is the EndpointArn of a mobile app and
-- device.
siEndpoint :: Lens' SubscribeInput (Maybe Text)
siEndpoint = lens _siEndpoint (\s a -> s { _siEndpoint = a })

-- | The protocol you want to use. Supported protocols include: http --
-- delivery of JSON-encoded message via HTTP POST https -- delivery of
-- JSON-encoded message via HTTPS POST email -- delivery of message via SMTP
-- email-json -- delivery of JSON-encoded message via SMTP sms -- delivery
-- of message via SMS sqs -- delivery of JSON-encoded message to an Amazon
-- SQS queue application -- delivery of JSON-encoded message to an
-- EndpointArn for a mobile app and device.
siProtocol :: Lens' SubscribeInput Text
siProtocol = lens _siProtocol (\s a -> s { _siProtocol = a })

-- | The ARN of the topic you want to subscribe to.
siTopicArn :: Lens' SubscribeInput Text
siTopicArn = lens _siTopicArn (\s a -> s { _siTopicArn = a })

instance ToPath SubscribeInput where
    toPath = const "/"

instance ToQuery SubscribeInput

newtype SubscribeResponse = SubscribeResponse
    { _srSubscriptionArn :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

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

instance AWSRequest SubscribeInput where
    type Sv SubscribeInput = SNS
    type Rs SubscribeInput = SubscribeResponse

    request  = post "Subscribe"
    response = const . xmlResponse $ \h x -> SubscribeResponse
newtype
