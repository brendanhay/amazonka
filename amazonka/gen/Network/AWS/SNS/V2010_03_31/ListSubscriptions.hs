{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.SNS.V2010_03_31.ListSubscriptions where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.SNS.V2010_03_31.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ListSubscriptions' request.
listSubscriptions :: ListSubscriptions
listSubscriptions = ListSubscriptions
    { _lsiNextToken = Nothing
    }

data ListSubscriptions = ListSubscriptions
    { _lsiNextToken :: Maybe Text
      -- ^ Token returned by the previous ListSubscriptions request.
    } deriving (Show, Generic)

makeLenses ''ListSubscriptions

instance ToQuery ListSubscriptions where
    toQuery = genericToQuery def

data ListSubscriptionsResponse = ListSubscriptionsResponse
    { _lsrNextToken :: Maybe Text
      -- ^ Token to pass along to the next ListSubscriptions request. This
      -- element is returned if there are more subscriptions to retrieve.
    , _lsrSubscriptions :: [Subscription]
      -- ^ A list of subscriptions.
    } deriving (Show, Generic)

makeLenses ''ListSubscriptionsResponse

instance AWSRequest ListSubscriptions where
    type Sv ListSubscriptions = SNS
    type Rs ListSubscriptions = ListSubscriptionsResponse

    request = post "ListSubscriptions"
    response _ = cursorResponse $ \hs xml ->
        pure ListSubscriptionsResponse
            <*> xml %|? "NextToken"
            <*> xml %| "SubscriptionsList"

instance AWSPager ListSubscriptions where
    next rq rs = (\x -> rq { _lsiNextToken = Just x })
        <$> (_lsrNextToken rs)
