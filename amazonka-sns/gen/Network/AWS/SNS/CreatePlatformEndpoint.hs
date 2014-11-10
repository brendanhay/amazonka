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

-- Module      : Network.AWS.SNS.CreatePlatformEndpoint
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates an endpoint for a device and mobile app on one of the supported
-- push notification services, such as GCM and APNS. CreatePlatformEndpoint
-- requires the PlatformApplicationArn that is returned from
-- CreatePlatformApplication. The EndpointArn that is returned when using
-- CreatePlatformEndpoint can then be used by the Publish action to send a
-- message to a mobile app or by the Subscribe action for subscription to a
-- topic. The CreatePlatformEndpoint action is idempotent, so if the requester
-- already owns an endpoint with the same device token and attributes, that
-- endpoint's ARN is returned without creating a new endpoint. For more
-- information, see Using Amazon SNS Mobile Push Notifications. When using
-- CreatePlatformEndpoint with Baidu, two attributes must be provided:
-- ChannelId and UserId. The token field must also contain the ChannelId. For
-- more information, see Creating an Amazon SNS Endpoint for Baidu.
module Network.AWS.SNS.CreatePlatformEndpoint
    (
    -- * Request
      CreatePlatformEndpointInput
    -- ** Request constructor
    , createPlatformEndpoint
    -- ** Request lenses
    , cpeiAttributes
    , cpeiCustomUserData
    , cpeiPlatformApplicationArn
    , cpeiToken

    -- * Response
    , CreateEndpointResponse
    -- ** Response constructor
    , createPlatformEndpointResponse
    -- ** Response lenses
    , cerEndpointArn
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SNS.Types

data CreatePlatformEndpointInput = CreatePlatformEndpointInput
    { _cpeiAttributes             :: Map Text Text
    , _cpeiCustomUserData         :: Maybe Text
    , _cpeiPlatformApplicationArn :: Text
    , _cpeiToken                  :: Text
    } deriving (Eq, Show, Generic)

-- | 'CreatePlatformEndpointInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpeiAttributes' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'cpeiCustomUserData' @::@ 'Maybe' 'Text'
--
-- * 'cpeiPlatformApplicationArn' @::@ 'Text'
--
-- * 'cpeiToken' @::@ 'Text'
--
createPlatformEndpoint :: Text -- ^ 'cpeiPlatformApplicationArn'
                       -> Text -- ^ 'cpeiToken'
                       -> CreatePlatformEndpointInput
createPlatformEndpoint p1 p2 = CreatePlatformEndpointInput
    { _cpeiPlatformApplicationArn = p1
    , _cpeiToken                  = p2
    , _cpeiCustomUserData         = Nothing
    , _cpeiAttributes             = mempty
    }

-- | For a list of attributes, see SetEndpointAttributes.
cpeiAttributes :: Lens' CreatePlatformEndpointInput (HashMap Text Text)
cpeiAttributes = lens _cpeiAttributes (\s a -> s { _cpeiAttributes = a })
    . _Map

-- | Arbitrary user data to associate with the endpoint. Amazon SNS does not
-- use this data. The data must be in UTF-8 format and less than 2KB.
cpeiCustomUserData :: Lens' CreatePlatformEndpointInput (Maybe Text)
cpeiCustomUserData =
    lens _cpeiCustomUserData (\s a -> s { _cpeiCustomUserData = a })

-- | PlatformApplicationArn returned from CreatePlatformApplication is used to
-- create a an endpoint.
cpeiPlatformApplicationArn :: Lens' CreatePlatformEndpointInput Text
cpeiPlatformApplicationArn =
    lens _cpeiPlatformApplicationArn
        (\s a -> s { _cpeiPlatformApplicationArn = a })

-- | Unique identifier created by the notification service for an app on a
-- device. The specific name for Token will vary, depending on which
-- notification service is being used. For example, when using APNS as the
-- notification service, you need the device token. Alternatively, when
-- using GCM or ADM, the device token equivalent is called the registration
-- ID.
cpeiToken :: Lens' CreatePlatformEndpointInput Text
cpeiToken = lens _cpeiToken (\s a -> s { _cpeiToken = a })

instance ToPath CreatePlatformEndpointInput where
    toPath = const "/"

instance ToQuery CreatePlatformEndpointInput

newtype CreateEndpointResponse = CreateEndpointResponse
    { _cerEndpointArn :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateEndpointResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cerEndpointArn' @::@ 'Maybe' 'Text'
--
createPlatformEndpointResponse :: CreateEndpointResponse
createPlatformEndpointResponse = CreateEndpointResponse
    { _cerEndpointArn = Nothing
    }

-- | EndpointArn returned from CreateEndpoint action.
cerEndpointArn :: Lens' CreateEndpointResponse (Maybe Text)
cerEndpointArn = lens _cerEndpointArn (\s a -> s { _cerEndpointArn = a })

instance AWSRequest CreatePlatformEndpointInput where
    type Sv CreatePlatformEndpointInput = SNS
    type Rs CreatePlatformEndpointInput = CreateEndpointResponse

    request  = post "CreatePlatformEndpoint"
    response = xmlResponse $ \h x -> CreateEndpointResponse
        <$> x %| "EndpointArn"
