{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.V2010_03_31.CreatePlatformEndpoint
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
-- more information, see Creating an Amazon SNS Endpoint for Baidu. POST
-- http://sns.us-west-2.amazonaws.com/ HTTP/1.1 ...
-- PlatformApplicationArn=arn%3Aaws%3Asns%3Aus-west-2%3A123456789012%3Aapp%2FGCM%2Fgcmpushapp
-- &amp;Action=CreatePlatformEndpoint &amp;SignatureMethod=HmacSHA256
-- &amp;CustomUserData=UserId%3D27576823
-- &amp;AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &amp;Token=APA91bGi7fFachkC1xjlqT66VYEucGHochmf1VQAr9k...jsM0PKPxKhddCzx6paEsyay9Zn3D4wNUJb8m6HZrBEXAMPLE
-- &amp;SignatureVersion=2 &amp;Version=2010-03-31
-- &amp;Signature=Rg5vXBS6OfgPtWkt1u32p1w14uiGh%2BKOicvXNWTEz2w%3D
-- &amp;Timestamp=2013-07-01T15%3A49%3A50.598Z HTTP/1.1 200 OK ...
-- &lt;CreatePlatformEndpointResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;CreatePlatformEndpointResult&gt;
-- &lt;EndpointArn&gt;arn:aws:sns:us-west-2:123456789012:endpoint/GCM/gcmpushapp/5e3e9847-3183-3f18-a7e8-671c3a57d4b3&lt;/EndpointArn&gt;
-- &lt;/CreatePlatformEndpointResult&gt; &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;6613341d-3e15-53f7-bf3c-7e56994ba278&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/CreatePlatformEndpointResponse&gt;.
module Network.AWS.SNS.V2010_03_31.CreatePlatformEndpoint
    (
    -- * Request
      CreatePlatformEndpoint
    -- ** Request constructor
    , mkCreatePlatformEndpoint
    -- ** Request lenses
    , cpePlatformApplicationArn
    , cpeToken
    , cpeCustomUserData
    , cpeAttributes

    -- * Response
    , CreatePlatformEndpointResponse
    -- ** Response constructor
    , mkCreatePlatformEndpointResponse
    -- ** Response lenses
    , cperEndpointArn
    ) where

import Network.AWS.Request.Query
import Network.AWS.SNS.V2010_03_31.Types
import Network.AWS.Prelude

-- | Input for CreatePlatformEndpoint action.
data CreatePlatformEndpoint = CreatePlatformEndpoint
    { _cpePlatformApplicationArn :: Text
    , _cpeToken :: Text
    , _cpeCustomUserData :: Maybe Text
    , _cpeAttributes :: Map Text Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreatePlatformEndpoint' request.
mkCreatePlatformEndpoint :: Text -- ^ 'cpePlatformApplicationArn'
                         -> Text -- ^ 'cpeToken'
                         -> CreatePlatformEndpoint
mkCreatePlatformEndpoint p1 p2 = CreatePlatformEndpoint
    { _cpePlatformApplicationArn = p1
    , _cpeToken = p2
    , _cpeCustomUserData = Nothing
    , _cpeAttributes = mempty
    }

-- | PlatformApplicationArn returned from CreatePlatformApplication is used to
-- create a an endpoint.
cpePlatformApplicationArn :: Lens' CreatePlatformEndpoint Text
cpePlatformApplicationArn =
    lens _cpePlatformApplicationArn
         (\s a -> s { _cpePlatformApplicationArn = a })

-- | Unique identifier created by the notification service for an app on a
-- device. The specific name for Token will vary, depending on which
-- notification service is being used. For example, when using APNS as the
-- notification service, you need the device token. Alternatively, when using
-- GCM or ADM, the device token equivalent is called the registration ID.
cpeToken :: Lens' CreatePlatformEndpoint Text
cpeToken = lens _cpeToken (\s a -> s { _cpeToken = a })

-- | Arbitrary user data to associate with the endpoint. Amazon SNS does not use
-- this data. The data must be in UTF-8 format and less than 2KB.
cpeCustomUserData :: Lens' CreatePlatformEndpoint (Maybe Text)
cpeCustomUserData =
    lens _cpeCustomUserData (\s a -> s { _cpeCustomUserData = a })

-- | For a list of attributes, see SetEndpointAttributes.
cpeAttributes :: Lens' CreatePlatformEndpoint (Map Text Text)
cpeAttributes = lens _cpeAttributes (\s a -> s { _cpeAttributes = a })

instance ToQuery CreatePlatformEndpoint where
    toQuery = genericQuery def

-- | Response from CreateEndpoint action.
newtype CreatePlatformEndpointResponse = CreatePlatformEndpointResponse
    { _cperEndpointArn :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreatePlatformEndpointResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkCreatePlatformEndpointResponse :: CreatePlatformEndpointResponse
mkCreatePlatformEndpointResponse = CreatePlatformEndpointResponse
    { _cperEndpointArn = Nothing
    }

-- | EndpointArn returned from CreateEndpoint action.
cperEndpointArn :: Lens' CreatePlatformEndpointResponse (Maybe Text)
cperEndpointArn = lens _cperEndpointArn (\s a -> s { _cperEndpointArn = a })

instance FromXML CreatePlatformEndpointResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreatePlatformEndpoint where
    type Sv CreatePlatformEndpoint = SNS
    type Rs CreatePlatformEndpoint = CreatePlatformEndpointResponse

    request = post "CreatePlatformEndpoint"
    response _ = xmlResponse
