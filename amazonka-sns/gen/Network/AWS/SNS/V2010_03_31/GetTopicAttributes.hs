{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.V2010_03_31.GetTopicAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns all of the properties of a topic. Topic properties returned might
-- differ based on the authorization of the user.
-- http://sns.us-east-1.amazonaws.com/
-- ?TopicArn=arn%3Aaws%3Asns%3Aus-east-1%3A123456789012%3AMy-Topic
-- &amp;Action=GetTopicAttributes &amp;SignatureVersion=2
-- &amp;SignatureMethod=HmacSHA256 &amp;Timestamp=2010-03-31T12%3A00%3A00.000Z
-- &amp;AWSAccessKeyId=(AWS Access Key Id)
-- &amp;Signature=92lBGRVq0%2BxhaACaBGqtdemy%2Bi9isfgyTljCbJM80Yk%3D
-- &lt;GetTopicAttributesResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;GetTopicAttributesResult&gt; &lt;Attributes&gt; &lt;entry&gt;
-- &lt;key&gt;Owner&lt;/key&gt; &lt;value&gt;123456789012&lt;/value&gt;
-- &lt;/entry&gt; &lt;entry&gt; &lt;key&gt;Policy&lt;/key&gt; &lt;value&gt;{
-- &amp;quot;Version&amp;quot;:&amp;quot;2008-10-17&amp;quot;,&amp;quot;Id&amp;quot;:&amp;quot;us-east-1/698519295917/test__default_policy_ID&amp;quot;,&amp;quot;Statement&amp;quot;
-- :
-- [{&amp;quot;Effect&amp;quot;:&amp;quot;Allow&amp;quot;,&amp;quot;Sid&amp;quot;:&amp;quot;us-east-1/698519295917/test__default_statement_ID&amp;quot;,&amp;quot;Principal&amp;quot;
-- : {&amp;quot;AWS&amp;quot;:
-- &amp;quot;*&amp;quot;},&amp;quot;Action&amp;quot;:[&amp;quot;SNS:GetTopicAttributes&amp;quot;,&amp;quot;SNS:SetTopicAttributes&amp;quot;,&amp;quot;SNS:AddPermission&amp;quot;,&amp;quot;SNS:RemovePermission&amp;quot;,&amp;quot;SNS:DeleteTopic&amp;quot;,&amp;quot;SNS:Subscribe&amp;quot;,&amp;quot;SNS:ListSubscriptionsByTopic&amp;quot;,&amp;quot;SNS:Publish&amp;quot;,&amp;quot;SNS:Receive&amp;quot;],&amp;quot;Resource&amp;quot;:&amp;quot;arn:aws:sns:us-east-1:698519295917:test&amp;quot;,&amp;quot;Condition&amp;quot;
-- : {&amp;quot;StringLike&amp;quot; : {&amp;quot;AWS:SourceArn&amp;quot;:
-- &amp;quot;arn:aws:*:*:698519295917:*&amp;quot;}}}]}&lt;/value&gt;
-- &lt;/entry&gt; &lt;entry&gt; &lt;key&gt;TopicArn&lt;/key&gt;
-- &lt;value&gt;arn:aws:sns:us-east-1:123456789012:My-Topic&lt;/value&gt;
-- &lt;/entry&gt; &lt;/Attributes&gt; &lt;/GetTopicAttributesResult&gt;
-- &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;057f074c-33a7-11df-9540-99d0768312d3&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/GetTopicAttributesResponse&gt;.
module Network.AWS.SNS.V2010_03_31.GetTopicAttributes
    (
    -- * Request
      GetTopicAttributes
    -- ** Request constructor
    , getTopicAttributes
    -- ** Request lenses
    , gtaiTopicArn

    -- * Response
    , GetTopicAttributesResponse
    -- ** Response lenses
    , gtarAttributes
    ) where

import Network.AWS.Request.Query
import Network.AWS.SNS.V2010_03_31.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'GetTopicAttributes' request.
getTopicAttributes :: Text -- ^ 'gtaiTopicArn'
                   -> GetTopicAttributes
getTopicAttributes p1 = GetTopicAttributes
    { _gtaiTopicArn = p1
    }
{-# INLINE getTopicAttributes #-}

data GetTopicAttributes = GetTopicAttributes
    { _gtaiTopicArn :: Text
      -- ^ The ARN of the topic whose properties you want to get.
    } deriving (Show, Generic)

-- | The ARN of the topic whose properties you want to get.
gtaiTopicArn :: Lens' GetTopicAttributes (Text)
gtaiTopicArn f x =
    f (_gtaiTopicArn x)
        <&> \y -> x { _gtaiTopicArn = y }
{-# INLINE gtaiTopicArn #-}

instance ToQuery GetTopicAttributes where
    toQuery = genericQuery def

data GetTopicAttributesResponse = GetTopicAttributesResponse
    { _gtarAttributes :: Map Text Text
      -- ^ A map of the topic's attributes. Attributes in this map include
      -- the following: TopicArn -- the topic's ARN Owner -- the AWS
      -- account ID of the topic's owner Policy -- the JSON serialization
      -- of the topic's access control policy DisplayName -- the
      -- human-readable name used in the "From" field for notifications to
      -- email and email-json endpoints SubscriptionsPending -- the number
      -- of subscriptions pending confirmation on this topic
      -- SubscriptionsConfirmed -- the number of confirmed subscriptions
      -- on this topic SubscriptionsDeleted -- the number of deleted
      -- subscriptions on this topic DeliveryPolicy -- the JSON
      -- serialization of the topic's delivery policy
      -- EffectiveDeliveryPolicy -- the JSON serialization of the
      -- effective delivery policy that takes into account system
      -- defaults.
    } deriving (Show, Generic)

-- | A map of the topic's attributes. Attributes in this map include the
-- following: TopicArn -- the topic's ARN Owner -- the AWS account ID of the
-- topic's owner Policy -- the JSON serialization of the topic's access
-- control policy DisplayName -- the human-readable name used in the "From"
-- field for notifications to email and email-json endpoints
-- SubscriptionsPending -- the number of subscriptions pending confirmation on
-- this topic SubscriptionsConfirmed -- the number of confirmed subscriptions
-- on this topic SubscriptionsDeleted -- the number of deleted subscriptions
-- on this topic DeliveryPolicy -- the JSON serialization of the topic's
-- delivery policy EffectiveDeliveryPolicy -- the JSON serialization of the
-- effective delivery policy that takes into account system defaults.
gtarAttributes :: Lens' GetTopicAttributesResponse (Map Text Text)
gtarAttributes f x =
    f (_gtarAttributes x)
        <&> \y -> x { _gtarAttributes = y }
{-# INLINE gtarAttributes #-}

instance FromXML GetTopicAttributesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetTopicAttributes where
    type Sv GetTopicAttributes = SNS
    type Rs GetTopicAttributes = GetTopicAttributesResponse

    request = post "GetTopicAttributes"
    response _ = xmlResponse
