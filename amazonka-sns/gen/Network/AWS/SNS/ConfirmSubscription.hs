{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS
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
-- https://sns.us-east-1.amazonaws.com/ ?Action=ConfirmSubscription
-- &amp;TopicArn=arn:aws:sns:us-east-1:123456789012:My-Topic
-- &amp;Token=51b2ff3edb475b7d91550e0ab6edf0c1de2a34e6ebaf6
-- c2262a001bcb7e051c43aa00022ceecce70bd2a67b2042da8d8
-- eb47fef7a4e4e942d23e7fa56146b9ee35da040b4b8af564cc4
-- 184a7391c834cb75d75c22981f776ad1ce8805e9bab29da2329
-- 985337bb8095627907b46c8577c8440556b6f86582a95475802
-- 6f41fc62041c4b3f67b0f5921232b5dae5aaca1 &lt;ConfirmSubscriptionResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;ConfirmSubscriptionResult&gt;
-- &lt;SubscriptionArn&gt;arn:aws:sns:us-east-1:123456789012:My-Topic:80289ba6-0fd4-4079-afb4-ce8c8260f0ca&lt;/SubscriptionArn&gt;
-- &lt;/ConfirmSubscriptionResult&gt; &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;7a50221f-3774-11df-a9b7-05d48da6f042&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/ConfirmSubscriptionResponse&gt;.
module Network.AWS.SNS
    (
    -- * Request
      ConfirmSubscription
    -- ** Request constructor
    , mkConfirmSubscription
    -- ** Request lenses
    , csTopicArn
    , csToken
    , csAuthenticateOnUnsubscribe

    -- * Response
    , ConfirmSubscriptionResponse
    -- ** Response constructor
    , mkConfirmSubscriptionResponse
    -- ** Response lenses
    , csrSubscriptionArn
    ) where

import Network.AWS.Request.Query
import Network.AWS.SNS.Types
import Network.AWS.Prelude

-- | Input for ConfirmSubscription action.
data ConfirmSubscription = ConfirmSubscription
    { _csTopicArn :: !Text
    , _csToken :: !Text
    , _csAuthenticateOnUnsubscribe :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ConfirmSubscription' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @TopicArn ::@ @Text@
--
-- * @Token ::@ @Text@
--
-- * @AuthenticateOnUnsubscribe ::@ @Maybe Text@
--
mkConfirmSubscription :: Text -- ^ 'csTopicArn'
                      -> Text -- ^ 'csToken'
                      -> ConfirmSubscription
mkConfirmSubscription p1 p2 = ConfirmSubscription
    { _csTopicArn = p1
    , _csToken = p2
    , _csAuthenticateOnUnsubscribe = Nothing
    }

-- | The ARN of the topic for which you wish to confirm a subscription.
csTopicArn :: Lens' ConfirmSubscription Text
csTopicArn = lens _csTopicArn (\s a -> s { _csTopicArn = a })

-- | Short-lived token sent to an endpoint during the Subscribe action.
csToken :: Lens' ConfirmSubscription Text
csToken = lens _csToken (\s a -> s { _csToken = a })

-- | Disallows unauthenticated unsubscribes of the subscription. If the value of
-- this parameter is true and the request has an AWS signature, then only the
-- topic owner and the subscription owner can unsubscribe the endpoint. The
-- unsubscribe action requires AWS authentication.
csAuthenticateOnUnsubscribe :: Lens' ConfirmSubscription (Maybe Text)
csAuthenticateOnUnsubscribe =
    lens _csAuthenticateOnUnsubscribe
         (\s a -> s { _csAuthenticateOnUnsubscribe = a })

instance ToQuery ConfirmSubscription where
    toQuery = genericQuery def

-- | Response for ConfirmSubscriptions action.
newtype ConfirmSubscriptionResponse = ConfirmSubscriptionResponse
    { _csrSubscriptionArn :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ConfirmSubscriptionResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SubscriptionArn ::@ @Maybe Text@
--
mkConfirmSubscriptionResponse :: ConfirmSubscriptionResponse
mkConfirmSubscriptionResponse = ConfirmSubscriptionResponse
    { _csrSubscriptionArn = Nothing
    }

-- | The ARN of the created subscription.
csrSubscriptionArn :: Lens' ConfirmSubscriptionResponse (Maybe Text)
csrSubscriptionArn =
    lens _csrSubscriptionArn (\s a -> s { _csrSubscriptionArn = a })

instance FromXML ConfirmSubscriptionResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ConfirmSubscription where
    type Sv ConfirmSubscription = SNS
    type Rs ConfirmSubscription = ConfirmSubscriptionResponse

    request = post "ConfirmSubscription"
    response _ = xmlResponse
