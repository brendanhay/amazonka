{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.SetSubscriptionAttributes
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
module Network.AWS.SNS.SetSubscriptionAttributes
    (
    -- * Request
      SetSubscriptionAttributes
    -- ** Request constructor
    , setSubscriptionAttributes
    -- ** Request lenses
    , ssaSubscriptionArn
    , ssaAttributeName
    , ssaAttributeValue

    -- * Response
    , SetSubscriptionAttributesResponse
    -- ** Response constructor
    , setSubscriptionAttributesResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.SNS.Types
import Network.AWS.Prelude

-- | Input for SetSubscriptionAttributes action.
data SetSubscriptionAttributes = SetSubscriptionAttributes
    { _ssaSubscriptionArn :: Text
    , _ssaAttributeName :: Text
    , _ssaAttributeValue :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SetSubscriptionAttributes' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SubscriptionArn ::@ @Text@
--
-- * @AttributeName ::@ @Text@
--
-- * @AttributeValue ::@ @Maybe Text@
--
setSubscriptionAttributes :: Text -- ^ 'ssaSubscriptionArn'
                          -> Text -- ^ 'ssaAttributeName'
                          -> SetSubscriptionAttributes
setSubscriptionAttributes p1 p2 = SetSubscriptionAttributes
    { _ssaSubscriptionArn = p1
    , _ssaAttributeName = p2
    , _ssaAttributeValue = Nothing
    }

-- | The ARN of the subscription to modify.
ssaSubscriptionArn :: Lens' SetSubscriptionAttributes Text
ssaSubscriptionArn =
    lens _ssaSubscriptionArn (\s a -> s { _ssaSubscriptionArn = a })

-- | The name of the attribute you want to set. Only a subset of the
-- subscriptions attributes are mutable. Valid values: DeliveryPolicy |
-- RawMessageDelivery.
ssaAttributeName :: Lens' SetSubscriptionAttributes Text
ssaAttributeName =
    lens _ssaAttributeName (\s a -> s { _ssaAttributeName = a })

-- | The new value for the attribute in JSON format.
ssaAttributeValue :: Lens' SetSubscriptionAttributes (Maybe Text)
ssaAttributeValue =
    lens _ssaAttributeValue (\s a -> s { _ssaAttributeValue = a })

instance ToQuery SetSubscriptionAttributes where
    toQuery = genericQuery def

data SetSubscriptionAttributesResponse = SetSubscriptionAttributesResponse
    deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SetSubscriptionAttributesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
setSubscriptionAttributesResponse :: SetSubscriptionAttributesResponse
setSubscriptionAttributesResponse = SetSubscriptionAttributesResponse

instance AWSRequest SetSubscriptionAttributes where
    type Sv SetSubscriptionAttributes = SNS
    type Rs SetSubscriptionAttributes = SetSubscriptionAttributesResponse

    request = post "SetSubscriptionAttributes"
    response _ = nullaryResponse SetSubscriptionAttributesResponse
