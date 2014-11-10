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

-- Module      : Network.AWS.SNS.GetEndpointAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Retrieves the endpoint attributes for a device on one of the supported push
-- notification services, such as GCM and APNS. For more information, see
-- Using Amazon SNS Mobile Push Notifications.
module Network.AWS.SNS.GetEndpointAttributes
    (
    -- * Request
      GetEndpointAttributesInput
    -- ** Request constructor
    , getEndpointAttributes
    -- ** Request lenses
    , geaiEndpointArn

    -- * Response
    , GetEndpointAttributesResponse
    -- ** Response constructor
    , getEndpointAttributesResponse
    -- ** Response lenses
    , gearAttributes
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SNS.Types

newtype GetEndpointAttributesInput = GetEndpointAttributesInput
    { _geaiEndpointArn :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'GetEndpointAttributesInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'geaiEndpointArn' @::@ 'Text'
--
getEndpointAttributes :: Text -- ^ 'geaiEndpointArn'
                      -> GetEndpointAttributesInput
getEndpointAttributes p1 = GetEndpointAttributesInput
    { _geaiEndpointArn = p1
    }

-- | EndpointArn for GetEndpointAttributes input.
geaiEndpointArn :: Lens' GetEndpointAttributesInput Text
geaiEndpointArn = lens _geaiEndpointArn (\s a -> s { _geaiEndpointArn = a })

instance ToPath GetEndpointAttributesInput where
    toPath = const "/"

instance ToQuery GetEndpointAttributesInput

newtype GetEndpointAttributesResponse = GetEndpointAttributesResponse
    { _gearAttributes :: Map Text Text
    } deriving (Eq, Show, Generic, Monoid)

-- | 'GetEndpointAttributesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gearAttributes' @::@ 'HashMap' 'Text' 'Text'
--
getEndpointAttributesResponse :: GetEndpointAttributesResponse
getEndpointAttributesResponse = GetEndpointAttributesResponse
    { _gearAttributes = mempty
    }

-- | Attributes include the following: CustomUserData -- arbitrary user data
-- to associate with the endpoint. Amazon SNS does not use this data. The
-- data must be in UTF-8 format and less than 2KB. Enabled -- flag that
-- enables/disables delivery to the endpoint. Amazon SNS will set this to
-- false when a notification service indicates to Amazon SNS that the
-- endpoint is invalid. Users can set it back to true, typically after
-- updating Token. Token -- device token, also referred to as a registration
-- id, for an app and mobile device. This is returned from the notification
-- service when an app and mobile device are registered with the
-- notification service.
gearAttributes :: Lens' GetEndpointAttributesResponse (HashMap Text Text)
gearAttributes = lens _gearAttributes (\s a -> s { _gearAttributes = a })
    . _Map

instance AWSRequest GetEndpointAttributesInput where
    type Sv GetEndpointAttributesInput = SNS
    type Rs GetEndpointAttributesInput = GetEndpointAttributesResponse

    request  = post "GetEndpointAttributes"
    response = xmlResponse $ \h x -> GetEndpointAttributesResponse
        <$> x %| "Attributes"
