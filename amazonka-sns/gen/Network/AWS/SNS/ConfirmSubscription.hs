{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
--
-- <http://docs.aws.amazon.com/sns/latest/api/API_ConfirmSubscription.html>
module Network.AWS.SNS.ConfirmSubscription
    (
    -- * Request
      ConfirmSubscription
    -- ** Request constructor
    , confirmSubscription
    -- ** Request lenses
    , csAuthenticateOnUnsubscribe
    , csToken
    , csTopicArn

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
import qualified GHC.Exts

data ConfirmSubscription = ConfirmSubscription
    { _csAuthenticateOnUnsubscribe :: Maybe Text
    , _csToken                     :: Text
    , _csTopicArn                  :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ConfirmSubscription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'csAuthenticateOnUnsubscribe' @::@ 'Maybe' 'Text'
--
-- * 'csToken' @::@ 'Text'
--
-- * 'csTopicArn' @::@ 'Text'
--
confirmSubscription :: Text -- ^ 'csTopicArn'
                    -> Text -- ^ 'csToken'
                    -> ConfirmSubscription
confirmSubscription p1 p2 = ConfirmSubscription
    { _csTopicArn                  = p1
    , _csToken                     = p2
    , _csAuthenticateOnUnsubscribe = Nothing
    }

-- | Disallows unauthenticated unsubscribes of the subscription. If the value
-- of this parameter is true and the request has an AWS signature, then only
-- the topic owner and the subscription owner can unsubscribe the endpoint.
-- The unsubscribe action requires AWS authentication.
csAuthenticateOnUnsubscribe :: Lens' ConfirmSubscription (Maybe Text)
csAuthenticateOnUnsubscribe =
    lens _csAuthenticateOnUnsubscribe
        (\s a -> s { _csAuthenticateOnUnsubscribe = a })

-- | Short-lived token sent to an endpoint during the Subscribe action.
csToken :: Lens' ConfirmSubscription Text
csToken = lens _csToken (\s a -> s { _csToken = a })

-- | The ARN of the topic for which you wish to confirm a subscription.
csTopicArn :: Lens' ConfirmSubscription Text
csTopicArn = lens _csTopicArn (\s a -> s { _csTopicArn = a })

newtype ConfirmSubscriptionResponse = ConfirmSubscriptionResponse
    { _csrSubscriptionArn :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

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

instance ToPath ConfirmSubscription where
    toPath = const "/"

instance ToQuery ConfirmSubscription

instance ToHeaders ConfirmSubscription

instance AWSRequest ConfirmSubscription where
    type Sv ConfirmSubscription = SNS
    type Rs ConfirmSubscription = ConfirmSubscriptionResponse

    request  = post "ConfirmSubscription"
    response = xmlResponse

instance FromXML ConfirmSubscriptionResponse where
    parseXML = withElement "ConfirmSubscriptionResult" $ \x ->
        ConfirmSubscriptionResponse
            <$> x .@? "SubscriptionArn"
