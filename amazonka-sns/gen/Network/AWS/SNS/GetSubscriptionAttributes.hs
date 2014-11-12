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

-- Module      : Network.AWS.SNS.GetSubscriptionAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns all of the properties of a subscription.
module Network.AWS.SNS.GetSubscriptionAttributes
    (
    -- * Request
      GetSubscriptionAttributesInput
    -- ** Request constructor
    , getSubscriptionAttributesInput
    -- ** Request lenses
    , gsaiSubscriptionArn

    -- * Response
    , GetSubscriptionAttributesResponse
    -- ** Response constructor
    , getSubscriptionAttributesResponse
    -- ** Response lenses
    , gsarAttributes
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SNS.Types

newtype GetSubscriptionAttributesInput = GetSubscriptionAttributesInput
    { _gsaiSubscriptionArn :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'GetSubscriptionAttributesInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsaiSubscriptionArn' @::@ 'Text'
--
getSubscriptionAttributesInput :: Text -- ^ 'gsaiSubscriptionArn'
                               -> GetSubscriptionAttributesInput
getSubscriptionAttributesInput p1 = GetSubscriptionAttributesInput
    { _gsaiSubscriptionArn = p1
    }

-- | The ARN of the subscription whose properties you want to get.
gsaiSubscriptionArn :: Lens' GetSubscriptionAttributesInput Text
gsaiSubscriptionArn =
    lens _gsaiSubscriptionArn (\s a -> s { _gsaiSubscriptionArn = a })

instance ToQuery GetSubscriptionAttributesInput

instance ToPath GetSubscriptionAttributesInput where
    toPath = const "/"

newtype GetSubscriptionAttributesResponse = GetSubscriptionAttributesResponse
    { _gsarAttributes :: Map Text Text
    } deriving (Eq, Show, Generic, Monoid, Semigroup, IsString)

instance IsList GetSubscriptionAttributesResponse
    type Item GetSubscriptionAttributesResponse = (Text, Text)

    fromList = GetSubscriptionAttributesResponse . fromList
    toList   = toList . _gsarAttributes

-- | 'GetSubscriptionAttributesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsarAttributes' @::@ 'HashMap' 'Text' 'Text'
--
getSubscriptionAttributesResponse :: GetSubscriptionAttributesResponse
getSubscriptionAttributesResponse = GetSubscriptionAttributesResponse
    { _gsarAttributes = mempty
    }

-- | A map of the subscription's attributes. Attributes in this map include
-- the following: SubscriptionArn -- the subscription's ARN TopicArn -- the
-- topic ARN that the subscription is associated with Owner -- the AWS
-- account ID of the subscription's owner ConfirmationWasAuthenticated --
-- true if the subscription confirmation request was authenticated
-- DeliveryPolicy -- the JSON serialization of the subscription's delivery
-- policy EffectiveDeliveryPolicy -- the JSON serialization of the effective
-- delivery policy that takes into account the topic delivery policy and
-- account system defaults.
gsarAttributes :: Lens' GetSubscriptionAttributesResponse (HashMap Text Text)
gsarAttributes = lens _gsarAttributes (\s a -> s { _gsarAttributes = a })
    . _Map

instance FromXML GetSubscriptionAttributesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "GetSubscriptionAttributesResponse"

instance AWSRequest GetSubscriptionAttributesInput where
    type Sv GetSubscriptionAttributesInput = SNS
    type Rs GetSubscriptionAttributesInput = GetSubscriptionAttributesResponse

    request  = post "GetSubscriptionAttributes"
    response = xmlResponse $ \h x -> GetSubscriptionAttributesResponse
        <$> x %| "Attributes"
