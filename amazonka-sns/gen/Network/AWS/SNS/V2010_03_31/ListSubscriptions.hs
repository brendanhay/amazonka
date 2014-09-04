{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.V2010_03_31.ListSubscriptions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of the requester's subscriptions. Each call returns a
-- limited list of subscriptions, up to 100. If there are more subscriptions,
-- a NextToken is also returned. Use the NextToken parameter in a new
-- ListSubscriptions call to get further results.
-- http://sns.us-east-1.amazonaws.com/ &amp;Action=ListSubscriptions
-- &amp;SignatureVersion=2 &amp;SignatureMethod=HmacSHA256
-- &amp;Timestamp=2010-03-31T12%3A00%3A00.000Z &amp;AWSAccessKeyId=(AWS Access
-- Key ID) &amp;Signature=SZmBxEPqfs9R7xxhSt6C1b7PnOEvg%2BSVyyMYJfLRFCA%3D
-- &lt;ListSubscriptionsResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;ListSubscriptionsResult&gt; &lt;Subscriptions&gt; &lt;member&gt;
-- &lt;TopicArn&gt;arn:aws:sns:us-east-1:698519295917:My-Topic&lt;/TopicArn&gt;
-- &lt;Protocol&gt;email&lt;/Protocol&gt;
-- &lt;SubscriptionArn&gt;arn:aws:sns:us-east-1:123456789012:My-Topic:80289ba6-0fd4-4079-afb4-ce8c8260f0ca&lt;/SubscriptionArn&gt;
-- &lt;Owner&gt;123456789012&lt;/Owner&gt;
-- &lt;Endpoint&gt;example@amazon.com&lt;/Endpoint&gt; &lt;/member&gt;
-- &lt;/Subscriptions&gt; &lt;/ListSubscriptionsResult&gt;
-- &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;384ac68d-3775-11df-8963-01868b7c937a&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/ListSubscriptionsResponse&gt;.
module Network.AWS.SNS.V2010_03_31.ListSubscriptions
    (
    -- * Request
      ListSubscriptions
    -- ** Request constructor
    , mkListSubscriptionsInput
    -- ** Request lenses
    , lsiNextToken

    -- * Response
    , ListSubscriptionsResponse
    -- ** Response lenses
    , lsrSubscriptions
    , lsrNextToken
    ) where

import Network.AWS.Request.Query
import Network.AWS.SNS.V2010_03_31.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListSubscriptions' request.
mkListSubscriptionsInput :: ListSubscriptions
mkListSubscriptionsInput = ListSubscriptions
    { _lsiNextToken = Nothing
    }
{-# INLINE mkListSubscriptionsInput #-}

newtype ListSubscriptions = ListSubscriptions
    { _lsiNextToken :: Maybe Text
      -- ^ Token returned by the previous ListSubscriptions request.
    } deriving (Show, Generic)

-- | Token returned by the previous ListSubscriptions request.
lsiNextToken :: Lens' ListSubscriptions (Maybe Text)
lsiNextToken = lens _lsiNextToken (\s a -> s { _lsiNextToken = a })
{-# INLINE lsiNextToken #-}

instance ToQuery ListSubscriptions where
    toQuery = genericQuery def

data ListSubscriptionsResponse = ListSubscriptionsResponse
    { _lsrSubscriptions :: [Subscription]
      -- ^ A list of subscriptions.
    , _lsrNextToken :: Maybe Text
      -- ^ Token to pass along to the next ListSubscriptions request. This
      -- element is returned if there are more subscriptions to retrieve.
    } deriving (Show, Generic)

-- | A list of subscriptions.
lsrSubscriptions :: Lens' ListSubscriptionsResponse ([Subscription])
lsrSubscriptions = lens _lsrSubscriptions (\s a -> s { _lsrSubscriptions = a })
{-# INLINE lsrSubscriptions #-}

-- | Token to pass along to the next ListSubscriptions request. This element is
-- returned if there are more subscriptions to retrieve.
lsrNextToken :: Lens' ListSubscriptionsResponse (Maybe Text)
lsrNextToken = lens _lsrNextToken (\s a -> s { _lsrNextToken = a })
{-# INLINE lsrNextToken #-}

instance FromXML ListSubscriptionsResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ListSubscriptions where
    type Sv ListSubscriptions = SNS
    type Rs ListSubscriptions = ListSubscriptionsResponse

    request = post "ListSubscriptions"
    response _ = xmlResponse

instance AWSPager ListSubscriptions where
    next rq rs = (\x -> rq { _lsiNextToken = Just x })
        <$> (_lsrNextToken rs)
