{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.SetTopicAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Allows a topic owner to set an attribute of the topic to a new value. The
-- following example sets the DisplayName attribute to MyTopicName
-- http://sns.us-east-1.amazonaws.com/ ?AttributeValue=MyTopicName
-- &amp;TopicArn=arn%3Aaws%3Asns%3Aus-east-1%3A123456789012%3AMy-Topic
-- &amp;AttributeName=DisplayName &amp;Action=SetTopicAttributes
-- &amp;SignatureVersion=2 &amp;SignatureMethod=HmacSHA256
-- &amp;Timestamp=2010-03-31T12%3A00%3A00.000Z &amp;AWSAccessKeyId=(AWS Access
-- Key Id) &amp;Signature=mQA3nJI%2BcmAIY7r8HCArGElSqPX5JG4UGzF4yo0RygE%3D The
-- following example sets the delivery policy to 5 total retries
-- http://sns.us-east-1.amazonaws.com/
-- ?AttributeValue={"http":{"defaultHealthyRetryPolicy":{"numRetries":5}}}
-- &amp;TopicArn=arn%3Aaws%3Asns%3Aus-east-1%3A123456789012%3AMy-Topic
-- &amp;AttributeName=DeliveryPolicy &amp;Action=SetTopicAttributes
-- &amp;SignatureVersion=2 &amp;SignatureMethod=HmacSHA256
-- &amp;Timestamp=2010-03-31T12%3A00%3A00.000Z &amp;AWSAccessKeyId=(AWS Access
-- Key Id) &amp;Signature=mQA3nJI%2BcmAIY7r8HCArGElSqPX5JG4UGzF4yo0RygE%3D The
-- JSON format for the DeliveryPolicy AttributeValue (linebreaks added for
-- readability): { "http": { "defaultHealthyRetryPolicy": { "minDelayTarget":
-- &lt;int&gt;, "maxDelayTarget": &lt;int&gt;, "numRetries": &lt;int&gt;,
-- "numMaxDelayRetries": &lt;int&gt;, "backoffFunction":
-- "&lt;linear|arithmetic|geometric|exponential&gt;" },
-- "disableSubscriptionOverrides": &lt;boolean&gt;, "defaultThrottlePolicy": {
-- "maxReceivesPerSecond": &lt;int&gt; } } &lt;SetTopicAttributesResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;a8763b99-33a7-11df-a9b7-05d48da6f042&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/SetTopicAttributesResponse&gt;.
module Network.AWS.SNS
    (
    -- * Request
      SetTopicAttributes
    -- ** Request constructor
    , mkSetTopicAttributes
    -- ** Request lenses
    , staTopicArn
    , staAttributeName
    , staAttributeValue

    -- * Response
    , SetTopicAttributesResponse
    -- ** Response constructor
    , mkSetTopicAttributesResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.SNS.Types
import Network.AWS.Prelude

-- | Input for SetTopicAttributes action.
data SetTopicAttributes = SetTopicAttributes
    { _staTopicArn :: !Text
    , _staAttributeName :: !Text
    , _staAttributeValue :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SetTopicAttributes' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @TopicArn ::@ @Text@
--
-- * @AttributeName ::@ @Text@
--
-- * @AttributeValue ::@ @Maybe Text@
--
mkSetTopicAttributes :: Text -- ^ 'staTopicArn'
                     -> Text -- ^ 'staAttributeName'
                     -> SetTopicAttributes
mkSetTopicAttributes p1 p2 = SetTopicAttributes
    { _staTopicArn = p1
    , _staAttributeName = p2
    , _staAttributeValue = Nothing
    }

-- | The ARN of the topic to modify.
staTopicArn :: Lens' SetTopicAttributes Text
staTopicArn = lens _staTopicArn (\s a -> s { _staTopicArn = a })

-- | The name of the attribute you want to set. Only a subset of the topic's
-- attributes are mutable. Valid values: Policy | DisplayName |
-- DeliveryPolicy.
staAttributeName :: Lens' SetTopicAttributes Text
staAttributeName =
    lens _staAttributeName (\s a -> s { _staAttributeName = a })

-- | The new value for the attribute.
staAttributeValue :: Lens' SetTopicAttributes (Maybe Text)
staAttributeValue =
    lens _staAttributeValue (\s a -> s { _staAttributeValue = a })

instance ToQuery SetTopicAttributes where
    toQuery = genericQuery def

data SetTopicAttributesResponse = SetTopicAttributesResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SetTopicAttributesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkSetTopicAttributesResponse :: SetTopicAttributesResponse
mkSetTopicAttributesResponse = SetTopicAttributesResponse

instance AWSRequest SetTopicAttributes where
    type Sv SetTopicAttributes = SNS
    type Rs SetTopicAttributes = SetTopicAttributesResponse

    request = post "SetTopicAttributes"
    response _ = nullaryResponse SetTopicAttributesResponse
