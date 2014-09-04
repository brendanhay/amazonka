{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.V2010_03_31.SetSubscriptionAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Allows a subscription owner to set an attribute of the topic to a new
-- value. The following example sets the delivery policy to 5 total retries
-- http://sns.us-east-1.amazonaws.com/
-- ?AttributeValue={"healthyRetryPolicy":{"numRetries":5}}
-- &amp;SubscriptionArn=arn%3Aaws%3Asns%3Aus-east-1%3A123456789012%3AMy-Topic%3A80289ba6-0fd4-4079-afb4-ce8c8260f0ca
-- &amp;AttributeName=DeliveryPolicy &amp;Action=SetSubscriptionAttributes
-- &amp;SignatureVersion=2 &amp;SignatureMethod=HmacSHA256
-- &amp;Timestamp=2010-03-31T12%3A00%3A00.000Z &amp;AWSAccessKeyId=(AWS Access
-- Key Id) &amp;Signature=mQA3nJI%2BcmAIY7r8HCArGElSqPX5JG4UGzF4yo0RygE%3D The
-- JSON format for the DeliveryPolicy AttributeValue (linebreaks added for
-- readability): { "healthyRetryPolicy": { "minDelayTarget": &lt;int&gt;,
-- "maxDelayTarget": &lt;int&gt;, "numRetries": &lt;int&gt;,
-- "numMaxDelayRetries": &lt;int&gt;, "backoffFunction":
-- "&lt;linear|arithmetic|geometric|exponential&gt;" }, "throttlePolicy": {
-- "maxReceivesPerSecond": &lt;int&gt; } }
-- &lt;SetSubscriptionAttributesResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;a8763b99-33a7-11df-a9b7-05d48da6f042&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/SetSubscriptionAttributesResponse&gt;.
module Network.AWS.SNS.V2010_03_31.SetSubscriptionAttributes
    (
    -- * Request
      SetSubscriptionAttributes
    -- ** Request constructor
    , mkSetSubscriptionAttributesInput
    -- ** Request lenses
    , ssaiSubscriptionArn
    , ssaiAttributeName
    , ssaiAttributeValue

    -- * Response
    , SetSubscriptionAttributesResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.SNS.V2010_03_31.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SetSubscriptionAttributes' request.
mkSetSubscriptionAttributesInput :: Text -- ^ 'ssaiSubscriptionArn'
                                 -> Text -- ^ 'ssaiAttributeName'
                                 -> SetSubscriptionAttributes
mkSetSubscriptionAttributesInput p1 p2 = SetSubscriptionAttributes
    { _ssaiSubscriptionArn = p1
    , _ssaiAttributeName = p2
    , _ssaiAttributeValue = Nothing
    }
{-# INLINE mkSetSubscriptionAttributesInput #-}

data SetSubscriptionAttributes = SetSubscriptionAttributes
    { _ssaiSubscriptionArn :: Text
      -- ^ The ARN of the subscription to modify.
    , _ssaiAttributeName :: Text
      -- ^ The name of the attribute you want to set. Only a subset of the
      -- subscriptions attributes are mutable. Valid values:
      -- DeliveryPolicy | RawMessageDelivery.
    , _ssaiAttributeValue :: Maybe Text
      -- ^ The new value for the attribute in JSON format.
    } deriving (Show, Generic)

-- | The ARN of the subscription to modify.
ssaiSubscriptionArn :: Lens' SetSubscriptionAttributes (Text)
ssaiSubscriptionArn = lens _ssaiSubscriptionArn (\s a -> s { _ssaiSubscriptionArn = a })
{-# INLINE ssaiSubscriptionArn #-}

-- | The name of the attribute you want to set. Only a subset of the
-- subscriptions attributes are mutable. Valid values: DeliveryPolicy |
-- RawMessageDelivery.
ssaiAttributeName :: Lens' SetSubscriptionAttributes (Text)
ssaiAttributeName = lens _ssaiAttributeName (\s a -> s { _ssaiAttributeName = a })
{-# INLINE ssaiAttributeName #-}

-- | The new value for the attribute in JSON format.
ssaiAttributeValue :: Lens' SetSubscriptionAttributes (Maybe Text)
ssaiAttributeValue = lens _ssaiAttributeValue (\s a -> s { _ssaiAttributeValue = a })
{-# INLINE ssaiAttributeValue #-}

instance ToQuery SetSubscriptionAttributes where
    toQuery = genericQuery def

data SetSubscriptionAttributesResponse = SetSubscriptionAttributesResponse
    deriving (Eq, Show, Generic)

instance AWSRequest SetSubscriptionAttributes where
    type Sv SetSubscriptionAttributes = SNS
    type Rs SetSubscriptionAttributes = SetSubscriptionAttributesResponse

    request = post "SetSubscriptionAttributes"
    response _ = nullaryResponse SetSubscriptionAttributesResponse
