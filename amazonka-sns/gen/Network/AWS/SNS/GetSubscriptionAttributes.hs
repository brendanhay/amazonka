{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.GetSubscriptionAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns all of the properties of a subscription.
--
-- <http://docs.aws.amazon.com/sns/latest/api/API_GetSubscriptionAttributes.html>
module Network.AWS.SNS.GetSubscriptionAttributes
    (
    -- * Request
      GetSubscriptionAttributes
    -- ** Request constructor
    , getSubscriptionAttributes
    -- ** Request lenses
    , gsaSubscriptionArn

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
import qualified GHC.Exts

newtype GetSubscriptionAttributes = GetSubscriptionAttributes
    { _gsaSubscriptionArn :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'GetSubscriptionAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gsaSubscriptionArn' @::@ 'Text'
--
getSubscriptionAttributes :: Text -- ^ 'gsaSubscriptionArn'
                          -> GetSubscriptionAttributes
getSubscriptionAttributes p1 = GetSubscriptionAttributes
    { _gsaSubscriptionArn = p1
    }

-- | The ARN of the subscription whose properties you want to get.
gsaSubscriptionArn :: Lens' GetSubscriptionAttributes Text
gsaSubscriptionArn =
    lens _gsaSubscriptionArn (\s a -> s { _gsaSubscriptionArn = a })

newtype GetSubscriptionAttributesResponse = GetSubscriptionAttributesResponse
    { _gsarAttributes :: EMap "entry" "key" "value" Text Text
    } deriving (Eq, Show, Monoid, Semigroup)

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

-- | A map of the subscription's attributes. Attributes in this map include the
-- following:
--
-- 'SubscriptionArn' -- the subscription's ARN  'TopicArn' -- the topic ARN that
-- the subscription is associated with  'Owner' -- the AWS account ID of the
-- subscription's owner  'ConfirmationWasAuthenticated' -- true if the
-- subscription confirmation request was authenticated  'DeliveryPolicy' -- the
-- JSON serialization of the subscription's delivery policy  'EffectiveDeliveryPolicy' -- the JSON serialization of the effective delivery policy that takes into
-- account the topic delivery policy and account system defaults
gsarAttributes :: Lens' GetSubscriptionAttributesResponse (HashMap Text Text)
gsarAttributes = lens _gsarAttributes (\s a -> s { _gsarAttributes = a }) . _EMap

instance ToPath GetSubscriptionAttributes where
    toPath = const "/"

instance ToQuery GetSubscriptionAttributes where
    toQuery GetSubscriptionAttributes{..} = mconcat
        [ "SubscriptionArn" =? _gsaSubscriptionArn
        ]

instance ToHeaders GetSubscriptionAttributes

instance AWSRequest GetSubscriptionAttributes where
    type Sv GetSubscriptionAttributes = SNS
    type Rs GetSubscriptionAttributes = GetSubscriptionAttributesResponse

    request  = post "GetSubscriptionAttributes"
    response = xmlResponse

instance FromXML GetSubscriptionAttributesResponse where
    parseXML = withElement "GetSubscriptionAttributesResult" $ \x -> GetSubscriptionAttributesResponse
        <$> x .@? "Attributes" .!@ mempty
