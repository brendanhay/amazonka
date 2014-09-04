{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.V2010_03_31.Unsubscribe
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a subscription. If the subscription requires authentication for
-- deletion, only the owner of the subscription or the topic's owner can
-- unsubscribe, and an AWS signature is required. If the Unsubscribe call does
-- not require authentication and the requester is not the subscription owner,
-- a final cancellation message is delivered to the endpoint, so that the
-- endpoint owner can easily resubscribe to the topic if the Unsubscribe
-- request was unintended. http://sns.us-east-1.amazonaws.com/
-- ?SubscriptionArn=arn%3Aaws%3Asns%3Aus-east-1%3A123456789012%3AMy-Topic%3A80289ba6-0fd4-4079-afb4-ce8c8260f0ca
-- &amp;Action=Unsubscribe &amp;SignatureVersion=2
-- &amp;SignatureMethod=HmacSHA256 &amp;Timestamp=2010-03-31T12%3A00%3A00.000Z
-- &amp;AWSAccessKeyId=(AWS Access Key ID)
-- &amp;Signature=e8IwhPzuWeMvPDVrN7jUVxasd3Wv2LuO8x6rE23VCv8%3D
-- &lt;UnsubscribeResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;18e0ac39-3776-11df-84c0-b93cc1666b84&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/UnsubscribeResponse&gt;.
module Network.AWS.SNS.V2010_03_31.Unsubscribe
    (
    -- * Request
      Unsubscribe
    -- ** Request constructor
    , mkUnsubscribeInput
    -- ** Request lenses
    , uiSubscriptionArn

    -- * Response
    , UnsubscribeResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.SNS.V2010_03_31.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'Unsubscribe' request.
mkUnsubscribeInput :: Text -- ^ 'uiSubscriptionArn'
                   -> Unsubscribe
mkUnsubscribeInput p1 = Unsubscribe
    { _uiSubscriptionArn = p1
    }
{-# INLINE mkUnsubscribeInput #-}

newtype Unsubscribe = Unsubscribe
    { _uiSubscriptionArn :: Text
      -- ^ The ARN of the subscription to be deleted.
    } deriving (Show, Generic)

-- | The ARN of the subscription to be deleted.
uiSubscriptionArn :: Lens' Unsubscribe (Text)
uiSubscriptionArn = lens _uiSubscriptionArn (\s a -> s { _uiSubscriptionArn = a })
{-# INLINE uiSubscriptionArn #-}

instance ToQuery Unsubscribe where
    toQuery = genericQuery def

data UnsubscribeResponse = UnsubscribeResponse
    deriving (Eq, Show, Generic)

instance AWSRequest Unsubscribe where
    type Sv Unsubscribe = SNS
    type Rs Unsubscribe = UnsubscribeResponse

    request = post "Unsubscribe"
    response _ = nullaryResponse UnsubscribeResponse
