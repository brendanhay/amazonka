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

-- Module      : Network.AWS.SNS.ConfirmSubscription
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Verifies an endpoint owner's intent to receive messages by validating the
-- token sent to the endpoint by an earlier Subscribe action. If the token is
-- valid, the action creates a new subscription and returns its Amazon
-- Resource Name (ARN). This call requires an AWS signature only when the
-- AuthenticateOnUnsubscribe flag is set to "true".
module Network.AWS.SNS.ConfirmSubscription
    (
    -- * Request
      ConfirmSubscriptionInput
    -- ** Request constructor
    , confirmSubscription
    -- ** Request lenses
    , csiAuthenticateOnUnsubscribe
    , csiToken
    , csiTopicArn

    -- * Response
    , ConfirmSubscriptionResponse
    -- ** Response constructor
    , confirmSubscriptionResponse
    -- ** Response lenses
    , csrSubscriptionArn
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SNS.Types

data ConfirmSubscriptionInput = ConfirmSubscriptionInput
    { _csiAuthenticateOnUnsubscribe :: Maybe Text
    , _csiToken                     :: Text
    , _csiTopicArn                  :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ConfirmSubscriptionInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csiAuthenticateOnUnsubscribe' @::@ 'Maybe' 'Text'
--
-- * 'csiToken' @::@ 'Text'
--
-- * 'csiTopicArn' @::@ 'Text'
--
confirmSubscription :: Text -- ^ 'csiTopicArn'
                    -> Text -- ^ 'csiToken'
                    -> ConfirmSubscriptionInput
confirmSubscription p1 p2 = ConfirmSubscriptionInput
    { _csiTopicArn                  = p1
    , _csiToken                     = p2
    , _csiAuthenticateOnUnsubscribe = Nothing
    }

-- | Disallows unauthenticated unsubscribes of the subscription. If the value
-- of this parameter is true and the request has an AWS signature, then only
-- the topic owner and the subscription owner can unsubscribe the endpoint.
-- The unsubscribe action requires AWS authentication.
csiAuthenticateOnUnsubscribe :: Lens' ConfirmSubscriptionInput (Maybe Text)
csiAuthenticateOnUnsubscribe =
    lens _csiAuthenticateOnUnsubscribe
        (\s a -> s { _csiAuthenticateOnUnsubscribe = a })

-- | Short-lived token sent to an endpoint during the Subscribe action.
csiToken :: Lens' ConfirmSubscriptionInput Text
csiToken = lens _csiToken (\s a -> s { _csiToken = a })

-- | The ARN of the topic for which you wish to confirm a subscription.
csiTopicArn :: Lens' ConfirmSubscriptionInput Text
csiTopicArn = lens _csiTopicArn (\s a -> s { _csiTopicArn = a })

instance ToPath ConfirmSubscriptionInput where
    toPath = const "/"

instance ToQuery ConfirmSubscriptionInput

newtype ConfirmSubscriptionResponse = ConfirmSubscriptionResponse
    { _csrSubscriptionArn :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ConfirmSubscriptionResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csrSubscriptionArn' @::@ 'Maybe' 'Text'
--
confirmSubscriptionResponse :: ConfirmSubscriptionResponse
confirmSubscriptionResponse = ConfirmSubscriptionResponse
    { _csrSubscriptionArn = Nothing
    }

-- | The ARN of the created subscription.
csrSubscriptionArn :: Lens' ConfirmSubscriptionResponse (Maybe Text)
csrSubscriptionArn =
    lens _csrSubscriptionArn (\s a -> s { _csrSubscriptionArn = a })

instance AWSRequest ConfirmSubscriptionInput where
    type Sv ConfirmSubscriptionInput = SNS
    type Rs ConfirmSubscriptionInput = ConfirmSubscriptionResponse

    request  = post "ConfirmSubscription"
    response = xmlResponse $ \h x -> ConfirmSubscriptionResponse
        <$> x %| "SubscriptionArn"
