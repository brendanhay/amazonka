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
--
-- <http://docs.aws.amazon.com/sns/latest/api/API_SetSubscriptionAttributes.html>
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
import qualified GHC.Exts

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

data SetSubscriptionAttributesResponse = SetSubscriptionAttributesResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'SetSubscriptionAttributesResponse' constructor.
setSubscriptionAttributesResponse :: SetSubscriptionAttributesResponse
setSubscriptionAttributesResponse = SetSubscriptionAttributesResponse

instance ToPath SetSubscriptionAttributes where
    toPath = const "/"

instance ToQuery SetSubscriptionAttributes

instance ToHeaders SetSubscriptionAttributes

instance AWSRequest SetSubscriptionAttributes where
    type Sv SetSubscriptionAttributes = SNS
    type Rs SetSubscriptionAttributes = SetSubscriptionAttributesResponse

    request  = post "SetSubscriptionAttributes"
    response = nullResponse SetSubscriptionAttributesResponse
