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

-- Module      : Network.AWS.SNS.SetSubscriptionAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Allows a subscription owner to set an attribute of the topic to a new
-- value.
module Network.AWS.SNS.SetSubscriptionAttributes
    (
    -- * Request
      SetSubscriptionAttributesInput
    -- ** Request constructor
    , setSubscriptionAttributesInput
    -- ** Request lenses
    , ssaiAttributeName
    , ssaiAttributeValue
    , ssaiSubscriptionArn

    -- * Response
    , SetSubscriptionAttributesResponse
    -- ** Response constructor
    , setSubscriptionAttributesResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SNS.Types

data SetSubscriptionAttributesInput = SetSubscriptionAttributesInput
    { _ssaiAttributeName   :: Text
    , _ssaiAttributeValue  :: Maybe Text
    , _ssaiSubscriptionArn :: Text
    } (Eq, Ord, Show, Generic)

-- | 'SetSubscriptionAttributesInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ssaiAttributeName' @::@ 'Text'
--
-- * 'ssaiAttributeValue' @::@ 'Maybe' 'Text'
--
-- * 'ssaiSubscriptionArn' @::@ 'Text'
--
setSubscriptionAttributesInput :: Text -- ^ 'ssaiSubscriptionArn'
                               -> Text -- ^ 'ssaiAttributeName'
                               -> SetSubscriptionAttributesInput
setSubscriptionAttributesInput p1 p2 = SetSubscriptionAttributesInput
    { _ssaiSubscriptionArn = p1
    , _ssaiAttributeName   = p2
    , _ssaiAttributeValue  = Nothing
    }

-- | The name of the attribute you want to set. Only a subset of the
-- subscriptions attributes are mutable. Valid values: DeliveryPolicy |
-- RawMessageDelivery.
ssaiAttributeName :: Lens' SetSubscriptionAttributesInput Text
ssaiAttributeName =
    lens _ssaiAttributeName (\s a -> s { _ssaiAttributeName = a })

-- | The new value for the attribute in JSON format.
ssaiAttributeValue :: Lens' SetSubscriptionAttributesInput (Maybe Text)
ssaiAttributeValue =
    lens _ssaiAttributeValue (\s a -> s { _ssaiAttributeValue = a })

-- | The ARN of the subscription to modify.
ssaiSubscriptionArn :: Lens' SetSubscriptionAttributesInput Text
ssaiSubscriptionArn =
    lens _ssaiSubscriptionArn (\s a -> s { _ssaiSubscriptionArn = a })
instance ToQuery SetSubscriptionAttributesInput

instance ToPath SetSubscriptionAttributesInput where
    toPath = const "/"

data SetSubscriptionAttributesResponse = SetSubscriptionAttributesResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'SetSubscriptionAttributesResponse' constructor.
setSubscriptionAttributesResponse :: SetSubscriptionAttributesResponse
setSubscriptionAttributesResponse = SetSubscriptionAttributesResponse

instance FromXML SetSubscriptionAttributesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SetSubscriptionAttributesResponse"

instance AWSRequest SetSubscriptionAttributesInput where
    type Sv SetSubscriptionAttributesInput = SNS
    type Rs SetSubscriptionAttributesInput = SetSubscriptionAttributesResponse

    request  = post "SetSubscriptionAttributes"
    response = nullaryResponse SetSubscriptionAttributesResponse
