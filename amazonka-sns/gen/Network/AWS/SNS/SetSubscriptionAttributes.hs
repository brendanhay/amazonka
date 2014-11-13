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
      SetSubscriptionAttributes
    -- ** Request constructor
    , setSubscriptionAttributes
    -- ** Request lenses
    , ssaAttributeName
    , ssaAttributeValue
    , ssaSubscriptionArn

    -- * Response
    , SetSubscriptionAttributesResponse
    -- ** Response constructor
    , setSubscriptionAttributesResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SNS.Types

data SetSubscriptionAttributes = SetSubscriptionAttributes
    { _ssaAttributeName   :: Text
    , _ssaAttributeValue  :: Maybe Text
    , _ssaSubscriptionArn :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'SetSubscriptionAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ssaAttributeName' @::@ 'Text'
--
-- * 'ssaAttributeValue' @::@ 'Maybe' 'Text'
--
-- * 'ssaSubscriptionArn' @::@ 'Text'
--
setSubscriptionAttributes :: Text -- ^ 'ssaSubscriptionArn'
                          -> Text -- ^ 'ssaAttributeName'
                          -> SetSubscriptionAttributes
setSubscriptionAttributes p1 p2 = SetSubscriptionAttributes
    { _ssaSubscriptionArn = p1
    , _ssaAttributeName   = p2
    , _ssaAttributeValue  = Nothing
    }

-- | The name of the attribute you want to set. Only a subset of the
-- subscriptions attributes are mutable. Valid values: DeliveryPolicy |
-- RawMessageDelivery.
ssaAttributeName :: Lens' SetSubscriptionAttributes Text
ssaAttributeName = lens _ssaAttributeName (\s a -> s { _ssaAttributeName = a })

-- | The new value for the attribute in JSON format.
ssaAttributeValue :: Lens' SetSubscriptionAttributes (Maybe Text)
ssaAttributeValue =
    lens _ssaAttributeValue (\s a -> s { _ssaAttributeValue = a })

-- | The ARN of the subscription to modify.
ssaSubscriptionArn :: Lens' SetSubscriptionAttributes Text
ssaSubscriptionArn =
    lens _ssaSubscriptionArn (\s a -> s { _ssaSubscriptionArn = a })

instance ToQuery SetSubscriptionAttributes

instance ToPath SetSubscriptionAttributes where
    toPath = const "/"

data SetSubscriptionAttributesResponse = SetSubscriptionAttributesResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'SetSubscriptionAttributesResponse' constructor.
setSubscriptionAttributesResponse :: SetSubscriptionAttributesResponse
setSubscriptionAttributesResponse = SetSubscriptionAttributesResponse

instance FromXML SetSubscriptionAttributesResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "SetSubscriptionAttributesResponse"

instance AWSRequest SetSubscriptionAttributes where
    type Sv SetSubscriptionAttributes = SNS
    type Rs SetSubscriptionAttributes = SetSubscriptionAttributesResponse

    request  = post "SetSubscriptionAttributes"
    response = nullaryResponse SetSubscriptionAttributesResponse
