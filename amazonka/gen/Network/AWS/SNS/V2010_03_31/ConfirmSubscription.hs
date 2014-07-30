{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.V2010_03_31.ConfirmSubscription
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
module Network.AWS.SNS.V2010_03_31.ConfirmSubscription where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.SNS.V2010_03_31.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

-- | Minimum specification for a 'ConfirmSubscription' request.
confirmSubscription :: Text -- ^ '_csiToken'
                    -> Text -- ^ '_csiTopicArn'
                    -> ConfirmSubscription
confirmSubscription p1 p2 = ConfirmSubscription
    { _csiToken = p1
    , _csiTopicArn = p2
    , _csiAuthenticateOnUnsubscribe = Nothing
    }

data ConfirmSubscription = ConfirmSubscription
    { _csiToken :: Text
      -- ^ Short-lived token sent to an endpoint during the Subscribe
      -- action.
    , _csiTopicArn :: Text
      -- ^ The ARN of the topic for which you wish to confirm a
      -- subscription.
    , _csiAuthenticateOnUnsubscribe :: Maybe Text
      -- ^ Disallows unauthenticated unsubscribes of the subscription. If
      -- the value of this parameter is true and the request has an AWS
      -- signature, then only the topic owner and the subscription owner
      -- can unsubscribe the endpoint. The unsubscribe action requires AWS
      -- authentication.
    } deriving (Generic)

instance ToQuery ConfirmSubscription where
    toQuery = genericToQuery def

instance AWSRequest ConfirmSubscription where
    type Sv ConfirmSubscription = SNS
    type Rs ConfirmSubscription = ConfirmSubscriptionResponse

    request = post "ConfirmSubscription"
    response _ = xmlResponse

data ConfirmSubscriptionResponse = ConfirmSubscriptionResponse
    { _csrSubscriptionArn :: Maybe Text
      -- ^ The ARN of the created subscription.
    } deriving (Generic)

instance FromXML ConfirmSubscriptionResponse where
    fromXMLOptions = xmlOptions
