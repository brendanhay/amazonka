{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.V2010_03_31.ListSubscriptionsByTopic
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of the subscriptions to a specific topic. Each call returns
-- a limited list of subscriptions, up to 100. If there are more
-- subscriptions, a NextToken is also returned. Use the NextToken parameter in
-- a new ListSubscriptionsByTopic call to get further results.
-- http://sns.us-east-1.amazonaws.com/
-- ?TopicArn=arn%3Aaws%3Asns%3Aus-east-1%3A123456789012%3AMy-Topic
-- &amp;Action=ListSubscriptionsByTopic &amp;SignatureVersion=2
-- &amp;SignatureMethod=HmacSHA256 &amp;Timestamp=2010-03-31T12%3A00%3A00.000Z
-- &amp;AWSAccessKeyId=(AWS Access Key ID)
-- &amp;Signature=SZmBxEPqfs9R7xxhSt6C1b7PnOEvg%2BSVyyMYJfLRFCA%3D
-- &lt;ListSubscriptionsByTopicResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;ListSubscriptionsByTopicResult&gt; &lt;Subscriptions&gt; &lt;member&gt;
-- &lt;TopicArn&gt;arn:aws:sns:us-east-1:123456789012:My-Topic&lt;/TopicArn&gt;
-- &lt;Protocol&gt;email&lt;/Protocol&gt;
-- &lt;SubscriptionArn&gt;arn:aws:sns:us-east-1:123456789012:My-Topic:80289ba6-0fd4-4079-afb4-ce8c8260f0ca&lt;/SubscriptionArn&gt;
-- &lt;Owner&gt;123456789012&lt;/Owner&gt;
-- &lt;Endpoint&gt;example@amazon.com&lt;/Endpoint&gt; &lt;/member&gt;
-- &lt;/Subscriptions&gt; &lt;/ListSubscriptionsByTopicResult&gt;
-- &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;b9275252-3774-11df-9540-99d0768312d3&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/ListSubscriptionsByTopicResponse&gt;.
module Network.AWS.SNS.V2010_03_31.ListSubscriptionsByTopic
    (
    -- * Request
      ListSubscriptionsByTopic
    -- ** Request constructor
    , listSubscriptionsByTopic
    -- ** Request lenses
    , lsbtiTopicArn
    , lsbtiNextToken

    -- * Response
    , ListSubscriptionsByTopicResponse
    -- ** Response lenses
    , lsbtrNextToken
    , lsbtrSubscriptions
    ) where

import Network.AWS.Request.Query
import Network.AWS.SNS.V2010_03_31.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ListSubscriptionsByTopic' request.
listSubscriptionsByTopic :: Text -- ^ 'lsbtiTopicArn'
                         -> ListSubscriptionsByTopic
listSubscriptionsByTopic p1 = ListSubscriptionsByTopic
    { _lsbtiTopicArn = p1
    , _lsbtiNextToken = Nothing
    }
{-# INLINE listSubscriptionsByTopic #-}

data ListSubscriptionsByTopic = ListSubscriptionsByTopic
    { _lsbtiTopicArn :: Text
      -- ^ The ARN of the topic for which you wish to find subscriptions.
    , _lsbtiNextToken :: Maybe Text
      -- ^ Token returned by the previous ListSubscriptionsByTopic request.
    } deriving (Show, Generic)

-- | The ARN of the topic for which you wish to find subscriptions.
lsbtiTopicArn :: Lens' ListSubscriptionsByTopic (Text)
lsbtiTopicArn f x =
    f (_lsbtiTopicArn x)
        <&> \y -> x { _lsbtiTopicArn = y }
{-# INLINE lsbtiTopicArn #-}

-- | Token returned by the previous ListSubscriptionsByTopic request.
lsbtiNextToken :: Lens' ListSubscriptionsByTopic (Maybe Text)
lsbtiNextToken f x =
    f (_lsbtiNextToken x)
        <&> \y -> x { _lsbtiNextToken = y }
{-# INLINE lsbtiNextToken #-}

instance ToQuery ListSubscriptionsByTopic where
    toQuery = genericQuery def

data ListSubscriptionsByTopicResponse = ListSubscriptionsByTopicResponse
    { _lsbtrNextToken :: Maybe Text
      -- ^ Token to pass along to the next ListSubscriptionsByTopic request.
      -- This element is returned if there are more subscriptions to
      -- retrieve.
    , _lsbtrSubscriptions :: [Subscription]
      -- ^ A list of subscriptions.
    } deriving (Show, Generic)

-- | Token to pass along to the next ListSubscriptionsByTopic request. This
-- element is returned if there are more subscriptions to retrieve.
lsbtrNextToken :: Lens' ListSubscriptionsByTopicResponse (Maybe Text)
lsbtrNextToken f x =
    f (_lsbtrNextToken x)
        <&> \y -> x { _lsbtrNextToken = y }
{-# INLINE lsbtrNextToken #-}

-- | A list of subscriptions.
lsbtrSubscriptions :: Lens' ListSubscriptionsByTopicResponse ([Subscription])
lsbtrSubscriptions f x =
    f (_lsbtrSubscriptions x)
        <&> \y -> x { _lsbtrSubscriptions = y }
{-# INLINE lsbtrSubscriptions #-}

instance FromXML ListSubscriptionsByTopicResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListSubscriptionsByTopic where
    type Sv ListSubscriptionsByTopic = SNS
    type Rs ListSubscriptionsByTopic = ListSubscriptionsByTopicResponse

    request = post "ListSubscriptionsByTopic"
    response _ = xmlResponse

instance AWSPager ListSubscriptionsByTopic where
    next rq rs = (\x -> rq { _lsbtiNextToken = Just x })
        <$> (_lsbtrNextToken rs)
