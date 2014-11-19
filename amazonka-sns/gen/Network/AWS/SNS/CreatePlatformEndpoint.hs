{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
--
-- <http://docs.aws.amazon.com/sns/latest/api/API_CreatePlatformEndpoint.html>
module Network.AWS.SNS.CreatePlatformEndpoint
    (
    -- * Request
      CreatePlatformEndpoint
    -- ** Request constructor
    , createPlatformEndpoint
    -- ** Request lenses
    , cpeAttributes
    , cpeCustomUserData
    , cpePlatformApplicationArn
    , cpeToken

    -- * Response
    , CreatePlatformEndpointResponse
    -- ** Response constructor
    , createPlatformEndpointResponse
    -- ** Response lenses
    , cperEndpointArn
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SNS.Types
import qualified GHC.Exts

data CreatePlatformEndpoint = CreatePlatformEndpoint
    { _cpeAttributes             :: Map Text Text
    , _cpeCustomUserData         :: Maybe Text
    , _cpePlatformApplicationArn :: Text
    , _cpeToken                  :: Text
    } deriving (Eq, Show, Generic)

-- | 'CreatePlatformEndpoint' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpeAttributes' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'cpeCustomUserData' @::@ 'Maybe' 'Text'
--
-- * 'cpePlatformApplicationArn' @::@ 'Text'
--
-- * 'cpeToken' @::@ 'Text'
--
createPlatformEndpoint :: Text -- ^ 'cpePlatformApplicationArn'
                       -> Text -- ^ 'cpeToken'
                       -> CreatePlatformEndpoint
createPlatformEndpoint p1 p2 = CreatePlatformEndpoint
    { _cpePlatformApplicationArn = p1
    , _cpeToken                  = p2
    , _cpeCustomUserData         = Nothing
    , _cpeAttributes             = mempty
    }

-- | For a list of attributes, see SetEndpointAttributes.
cpeAttributes :: Lens' CreatePlatformEndpoint (HashMap Text Text)
cpeAttributes = lens _cpeAttributes (\s a -> s { _cpeAttributes = a })
    . _Map

-- | Arbitrary user data to associate with the endpoint. Amazon SNS does not
-- use this data. The data must be in UTF-8 format and less than 2KB.
cpeCustomUserData :: Lens' CreatePlatformEndpoint (Maybe Text)
cpeCustomUserData =
    lens _cpeCustomUserData (\s a -> s { _cpeCustomUserData = a })

-- | PlatformApplicationArn returned from CreatePlatformApplication is used to
-- create a an endpoint.
cpePlatformApplicationArn :: Lens' CreatePlatformEndpoint Text
cpePlatformApplicationArn =
    lens _cpePlatformApplicationArn
        (\s a -> s { _cpePlatformApplicationArn = a })

-- | Unique identifier created by the notification service for an app on a
-- device. The specific name for Token will vary, depending on which
-- notification service is being used. For example, when using APNS as the
-- notification service, you need the device token. Alternatively, when
-- using GCM or ADM, the device token equivalent is called the registration
-- ID.
cpeToken :: Lens' CreatePlatformEndpoint Text
cpeToken = lens _cpeToken (\s a -> s { _cpeToken = a })

newtype CreatePlatformEndpointResponse = CreatePlatformEndpointResponse
    { _cperEndpointArn :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'CreatePlatformEndpointResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cperEndpointArn' @::@ 'Maybe' 'Text'
--
createPlatformEndpointResponse :: CreatePlatformEndpointResponse
createPlatformEndpointResponse = CreatePlatformEndpointResponse
    { _cperEndpointArn = Nothing
    }

-- | EndpointArn returned from CreateEndpoint action.
cperEndpointArn :: Lens' CreatePlatformEndpointResponse (Maybe Text)
cperEndpointArn = lens _cperEndpointArn (\s a -> s { _cperEndpointArn = a })

instance ToPath CreatePlatformEndpoint where
    toPath = const "/"

instance ToQuery CreatePlatformEndpoint

instance ToHeaders CreatePlatformEndpoint

instance AWSRequest CreatePlatformEndpoint where
    type Sv CreatePlatformEndpoint = SNS
    type Rs CreatePlatformEndpoint = CreatePlatformEndpointResponse

    request  = post "CreatePlatformEndpoint"
    response = xmlResponse

instance FromXML CreatePlatformEndpointResponse where
    parseXML = withElement "CreatePlatformEndpointResult" $ \x ->
            <$> x .@? "EndpointArn"
