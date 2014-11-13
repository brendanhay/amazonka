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

-- Module      : Network.AWS.SNS.SetEndpointAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets the attributes for an endpoint for a device on one of the supported
-- push notification services, such as GCM and APNS. For more information, see
-- Using Amazon SNS Mobile Push Notifications.
module Network.AWS.SNS.SetEndpointAttributes
    (
    -- * Request
      SetEndpointAttributes
    -- ** Request constructor
    , setEndpointAttributes
    -- ** Request lenses
    , seaAttributes
    , seaEndpointArn

    -- * Response
    , SetEndpointAttributesResponse
    -- ** Response constructor
    , setEndpointAttributesResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SNS.Types
import qualified GHC.Exts

data SetEndpointAttributes = SetEndpointAttributes
    { _seaAttributes  :: Map Text Text
    , _seaEndpointArn :: Text
    } deriving (Eq, Show, Generic)

-- | 'SetEndpointAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'seaAttributes' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'seaEndpointArn' @::@ 'Text'
--
setEndpointAttributes :: Text -- ^ 'seaEndpointArn'
                      -> SetEndpointAttributes
setEndpointAttributes p1 = SetEndpointAttributes
    { _seaEndpointArn = p1
    , _seaAttributes  = mempty
    }

-- | A map of the endpoint attributes. Attributes in this map include the
-- following: CustomUserData -- arbitrary user data to associate with the
-- endpoint. Amazon SNS does not use this data. The data must be in UTF-8
-- format and less than 2KB. Enabled -- flag that enables/disables delivery
-- to the endpoint. Amazon SNS will set this to false when a notification
-- service indicates to Amazon SNS that the endpoint is invalid. Users can
-- set it back to true, typically after updating Token. Token -- device
-- token, also referred to as a registration id, for an app and mobile
-- device. This is returned from the notification service when an app and
-- mobile device are registered with the notification service.
seaAttributes :: Lens' SetEndpointAttributes (HashMap Text Text)
seaAttributes = lens _seaAttributes (\s a -> s { _seaAttributes = a })
    . _Map

-- | EndpointArn used for SetEndpointAttributes action.
seaEndpointArn :: Lens' SetEndpointAttributes Text
seaEndpointArn = lens _seaEndpointArn (\s a -> s { _seaEndpointArn = a })

instance ToQuery SetEndpointAttributes

instance ToPath SetEndpointAttributes where
    toPath = const "/"

data SetEndpointAttributesResponse = SetEndpointAttributesResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'SetEndpointAttributesResponse' constructor.
setEndpointAttributesResponse :: SetEndpointAttributesResponse
setEndpointAttributesResponse = SetEndpointAttributesResponse

instance AWSRequest SetEndpointAttributes where
    type Sv SetEndpointAttributes = SNS
    type Rs SetEndpointAttributes = SetEndpointAttributesResponse

    request  = post "SetEndpointAttributes"
    response = nullaryResponse SetEndpointAttributesResponse
