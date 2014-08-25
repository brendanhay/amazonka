{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.V2010_03_31.GetSubscriptionAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns all of the properties of a subscription.
-- http://sns.us-east-1.amazonaws.com/
-- ?SubscriptionArn=arn%3Aaws%3Asns%3Aus-east-1%3A123456789012%3AMy-Topic%3A80289ba6-0fd4-4079-afb4-ce8c8260f0ca
-- &amp;Action=GetSubscriptionAttributes &amp;SignatureVersion=2
-- &amp;SignatureMethod=HmacSHA256 &amp;Timestamp=2010-03-31T12%3A00%3A00.000Z
-- &amp;AWSAccessKeyId=(AWS Access Key Id)
-- &amp;Signature=92lBGRVq0%2BxhaACaBGqtdemy%2Bi9isfgyTljCbJM80Yk%3D
-- &lt;GetSubscriptionAttributesResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;GetSubscriptionAttributesResult&gt; &lt;Attributes&gt; &lt;entry&gt;
-- &lt;key&gt;Owner&lt;/key&gt; &lt;value&gt;123456789012&lt;/value&gt;
-- &lt;/entry&gt; &lt;entry&gt; &lt;key&gt;DeliveryPolicy&lt;/key&gt;
-- &lt;value&gt;{&amp;quot;healthyRetryPolicy&amp;quot;:{&amp;quot;numRetries&amp;quot;:10}}&lt;/value&gt;
-- &lt;/entry&gt; &lt;entry&gt; &lt;key&gt;SubscriptionArn&lt;/key&gt;
-- &lt;value&gt;arn:aws:sns:us-east-1:123456789012:My-Topic:80289ba6-0fd4-4079-afb4-ce8c8260f0ca&lt;/value&gt;
-- &lt;/entry&gt; &lt;/Attributes&gt; &lt;/GetSubscriptionAttributesResult&gt;
-- &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;057f074c-33a7-11df-9540-99d0768312d3&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/GetTopicAttributesResponse&gt;.
module Network.AWS.SNS.V2010_03_31.GetSubscriptionAttributes where

import Network.AWS.Request.Query
import Network.AWS.SNS.V2010_03_31.Types
import Network.AWS.Prelude

data GetSubscriptionAttributes = GetSubscriptionAttributes
    { _gsaiSubscriptionArn :: Text
      -- ^ The ARN of the subscription whose properties you want to get.
    } deriving (Show, Generic)

makeLenses ''GetSubscriptionAttributes

instance ToQuery GetSubscriptionAttributes where
    toQuery = genericQuery def

data GetSubscriptionAttributesResponse = GetSubscriptionAttributesResponse
    { _gsarAttributes :: Map Text Text
      -- ^ A map of the subscription's attributes. Attributes in this map
      -- include the following: SubscriptionArn -- the subscription's ARN
      -- TopicArn -- the topic ARN that the subscription is associated
      -- with Owner -- the AWS account ID of the subscription's owner
      -- ConfirmationWasAuthenticated -- true if the subscription
      -- confirmation request was authenticated DeliveryPolicy -- the JSON
      -- serialization of the subscription's delivery policy
      -- EffectiveDeliveryPolicy -- the JSON serialization of the
      -- effective delivery policy that takes into account the topic
      -- delivery policy and account system defaults.
    } deriving (Show, Generic)

makeLenses ''GetSubscriptionAttributesResponse

instance FromXML GetSubscriptionAttributesResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest GetSubscriptionAttributes where
    type Sv GetSubscriptionAttributes = SNS
    type Rs GetSubscriptionAttributes = GetSubscriptionAttributesResponse

    request = post "GetSubscriptionAttributes"
    response _ = xmlResponse
