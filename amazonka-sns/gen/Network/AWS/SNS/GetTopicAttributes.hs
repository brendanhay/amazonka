{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.SNS.GetTopicAttributes
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
module Network.AWS.SNS.GetTopicAttributes
    (
    -- * Request
      GetTopicAttributesInput
    -- ** Request constructor
    , getTopicAttributesInput
    -- ** Request lenses
    , gtaiTopicArn

    -- * Response
    , GetTopicAttributesResponse
    -- ** Response constructor
    , getTopicAttributesResponse
    -- ** Response lenses
    , gtarAttributes
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SNS.Types

newtype GetTopicAttributesInput = GetTopicAttributesInput
    { _gtaiTopicArn :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'GetTopicAttributesInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gtaiTopicArn' @::@ 'Text'
--
getTopicAttributesInput :: Text -- ^ 'gtaiTopicArn'
                        -> GetTopicAttributesInput
getTopicAttributesInput p1 = GetTopicAttributesInput
    { _gtaiTopicArn = p1
    }

-- | The ARN of the topic whose properties you want to get.
gtaiTopicArn :: Lens' GetTopicAttributesInput Text
gtaiTopicArn = lens _gtaiTopicArn (\s a -> s { _gtaiTopicArn = a })
instance ToQuery GetTopicAttributesInput

instance ToPath GetTopicAttributesInput where
    toPath = const "/"

newtype GetTopicAttributesResponse = GetTopicAttributesResponse
    { _gtarAttributes :: Map Text Text
    } deriving (Eq, Show, Generic, Monoid)

-- | 'GetTopicAttributesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gtarAttributes' @::@ 'HashMap' 'Text' 'Text'
--
getTopicAttributesResponse :: GetTopicAttributesResponse
getTopicAttributesResponse = GetTopicAttributesResponse
    { _gtarAttributes = mempty
    }

-- | A map of the topic's attributes. Attributes in this map include the
-- following: TopicArn -- the topic's ARN Owner -- the AWS account ID of the
-- topic's owner Policy -- the JSON serialization of the topic's access
-- control policy DisplayName -- the human-readable name used in the "From"
-- field for notifications to email and email-json endpoints
-- SubscriptionsPending -- the number of subscriptions pending confirmation
-- on this topic SubscriptionsConfirmed -- the number of confirmed
-- subscriptions on this topic SubscriptionsDeleted -- the number of deleted
-- subscriptions on this topic DeliveryPolicy -- the JSON serialization of
-- the topic's delivery policy EffectiveDeliveryPolicy -- the JSON
-- serialization of the effective delivery policy that takes into account
-- system defaults.
gtarAttributes :: Lens' GetTopicAttributesResponse (HashMap Text Text)
gtarAttributes = lens _gtarAttributes (\s a -> s { _gtarAttributes = a })
    . _Map
instance FromXML GetTopicAttributesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "GetTopicAttributesResponse"

instance AWSRequest GetTopicAttributesInput where
    type Sv GetTopicAttributesInput = SNS
    type Rs GetTopicAttributesInput = GetTopicAttributesResponse

    request  = post "GetTopicAttributes"
    response = xmlResponse $ \h x -> GetTopicAttributesResponse
        <$> x %| "Attributes"
