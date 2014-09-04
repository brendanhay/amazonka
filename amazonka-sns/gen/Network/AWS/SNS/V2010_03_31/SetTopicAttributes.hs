{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.V2010_03_31.SetTopicAttributes
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
module Network.AWS.SNS.V2010_03_31.SetTopicAttributes
    (
    -- * Request
      SetTopicAttributes
    -- ** Request constructor
    , setTopicAttributes
    -- ** Request lenses
    , staiAttributeName
    , staiTopicArn
    , staiAttributeValue

    -- * Response
    , SetTopicAttributesResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.SNS.V2010_03_31.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'SetTopicAttributes' request.
setTopicAttributes :: Text -- ^ 'staiAttributeName'
                   -> Text -- ^ 'staiTopicArn'
                   -> SetTopicAttributes
setTopicAttributes p1 p2 = SetTopicAttributes
    { _staiAttributeName = p1
    , _staiTopicArn = p2
    , _staiAttributeValue = Nothing
    }
{-# INLINE setTopicAttributes #-}

data SetTopicAttributes = SetTopicAttributes
    { _staiAttributeName :: Text
      -- ^ The name of the attribute you want to set. Only a subset of the
      -- topic's attributes are mutable. Valid values: Policy |
      -- DisplayName | DeliveryPolicy.
    , _staiTopicArn :: Text
      -- ^ The ARN of the topic to modify.
    , _staiAttributeValue :: Maybe Text
      -- ^ The new value for the attribute.
    } deriving (Show, Generic)

-- | The name of the attribute you want to set. Only a subset of the topic's
-- attributes are mutable. Valid values: Policy | DisplayName |
-- DeliveryPolicy.
staiAttributeName :: Lens' SetTopicAttributes (Text)
staiAttributeName f x =
    f (_staiAttributeName x)
        <&> \y -> x { _staiAttributeName = y }
{-# INLINE staiAttributeName #-}

-- | The ARN of the topic to modify.
staiTopicArn :: Lens' SetTopicAttributes (Text)
staiTopicArn f x =
    f (_staiTopicArn x)
        <&> \y -> x { _staiTopicArn = y }
{-# INLINE staiTopicArn #-}

-- | The new value for the attribute.
staiAttributeValue :: Lens' SetTopicAttributes (Maybe Text)
staiAttributeValue f x =
    f (_staiAttributeValue x)
        <&> \y -> x { _staiAttributeValue = y }
{-# INLINE staiAttributeValue #-}

instance ToQuery SetTopicAttributes where
    toQuery = genericQuery def

data SetTopicAttributesResponse = SetTopicAttributesResponse
    deriving (Eq, Show, Generic)

instance AWSRequest SetTopicAttributes where
    type Sv SetTopicAttributes = SNS
    type Rs SetTopicAttributes = SetTopicAttributesResponse

    request = post "SetTopicAttributes"
    response _ = nullaryResponse SetTopicAttributesResponse
