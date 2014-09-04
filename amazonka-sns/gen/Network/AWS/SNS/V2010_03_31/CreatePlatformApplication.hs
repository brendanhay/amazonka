{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SNS.V2010_03_31.CreatePlatformApplication
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a platform application object for one of the supported push
-- notification services, such as APNS and GCM, to which devices and mobile
-- apps may register. You must specify PlatformPrincipal and
-- PlatformCredential attributes when using the CreatePlatformApplication
-- action. The PlatformPrincipal is received from the notification service.
-- For APNS/APNS_SANDBOX, PlatformPrincipal is "SSL certificate". For GCM,
-- PlatformPrincipal is not applicable. For ADM, PlatformPrincipal is "client
-- id". The PlatformCredential is also received from the notification service.
-- For APNS/APNS_SANDBOX, PlatformCredential is "private key". For GCM,
-- PlatformCredential is "API key". For ADM, PlatformCredential is "client
-- secret". The PlatformApplicationArn that is returned when using
-- CreatePlatformApplication is then used as an attribute for the
-- CreatePlatformEndpoint action. For more information, see Using Amazon SNS
-- Mobile Push Notifications. POST http://sns.us-west-2.amazonaws.com/
-- HTTP/1.1 ... Attributes.entry.2.key=PlatformPrincipal
-- &amp;SignatureMethod=HmacSHA256
-- &amp;Attributes.entry.1.value=AIzaSyClE2lcV2zEKTLYYo645zfk2jhQPFeyxDo
-- &amp;Attributes.entry.2.value=There+is+no+principal+for+GCM
-- &amp;AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE
-- &amp;Signature=82sHzg1Wfbgisw3i%2BHA2OgBmRktsqUKFinknkq3u%2FQ4%3D
-- &amp;Timestamp=2013-07-01T15%3A49%3A50.354Z &amp;Name=gcmpushapp
-- &amp;Attributes.entry.1.key=PlatformCredential
-- &amp;Action=CreatePlatformApplication &amp;Version=2010-03-31
-- &amp;SignatureVersion=2 &amp;Platform=GCM HTTP/1.1 200 OK ...
-- &lt;CreatePlatformApplicationResponse
-- xmlns="http://sns.amazonaws.com/doc/2010-03-31/"&gt;
-- &lt;CreatePlatformApplicationResult&gt;
-- &lt;PlatformApplicationArn&gt;arn:aws:sns:us-west-2:123456789012:app/GCM/gcmpushapp&lt;/PlatformApplicationArn&gt;
-- &lt;/CreatePlatformApplicationResult&gt; &lt;ResponseMetadata&gt;
-- &lt;RequestId&gt;b6f0e78b-e9d4-5a0e-b973-adc04e8a4ff9&lt;/RequestId&gt;
-- &lt;/ResponseMetadata&gt; &lt;/CreatePlatformApplicationResponse&gt;.
module Network.AWS.SNS.V2010_03_31.CreatePlatformApplication
    (
    -- * Request
      CreatePlatformApplication
    -- ** Request constructor
    , createPlatformApplication
    -- ** Request lenses
    , cpaiAttributes
    , cpaiName
    , cpaiPlatform

    -- * Response
    , CreatePlatformApplicationResponse
    -- ** Response lenses
    , cparPlatformApplicationArn
    ) where

import Network.AWS.Request.Query
import Network.AWS.SNS.V2010_03_31.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreatePlatformApplication' request.
createPlatformApplication :: Map Text Text -- ^ 'cpaiAttributes'
                          -> Text -- ^ 'cpaiName'
                          -> Text -- ^ 'cpaiPlatform'
                          -> CreatePlatformApplication
createPlatformApplication p1 p2 p3 = CreatePlatformApplication
    { _cpaiAttributes = p1
    , _cpaiName = p2
    , _cpaiPlatform = p3
    }
{-# INLINE createPlatformApplication #-}

data CreatePlatformApplication = CreatePlatformApplication
    { _cpaiAttributes :: Map Text Text
      -- ^ For a list of attributes, see SetPlatformApplicationAttributes.
    , _cpaiName :: Text
      -- ^ Application names must be made up of only uppercase and lowercase
      -- ASCII letters, numbers, underscores, hyphens, and periods, and
      -- must be between 1 and 256 characters long.
    , _cpaiPlatform :: Text
      -- ^ The following platforms are supported: ADM (Amazon Device
      -- Messaging), APNS (Apple Push Notification Service), APNS_SANDBOX,
      -- and GCM (Google Cloud Messaging).
    } deriving (Show, Generic)

-- | For a list of attributes, see SetPlatformApplicationAttributes.
cpaiAttributes :: Lens' CreatePlatformApplication (Map Text Text)
cpaiAttributes f x =
    f (_cpaiAttributes x)
        <&> \y -> x { _cpaiAttributes = y }
{-# INLINE cpaiAttributes #-}

-- | Application names must be made up of only uppercase and lowercase ASCII
-- letters, numbers, underscores, hyphens, and periods, and must be between 1
-- and 256 characters long.
cpaiName :: Lens' CreatePlatformApplication (Text)
cpaiName f x =
    f (_cpaiName x)
        <&> \y -> x { _cpaiName = y }
{-# INLINE cpaiName #-}

-- | The following platforms are supported: ADM (Amazon Device Messaging), APNS
-- (Apple Push Notification Service), APNS_SANDBOX, and GCM (Google Cloud
-- Messaging).
cpaiPlatform :: Lens' CreatePlatformApplication (Text)
cpaiPlatform f x =
    f (_cpaiPlatform x)
        <&> \y -> x { _cpaiPlatform = y }
{-# INLINE cpaiPlatform #-}

instance ToQuery CreatePlatformApplication where
    toQuery = genericQuery def

data CreatePlatformApplicationResponse = CreatePlatformApplicationResponse
    { _cparPlatformApplicationArn :: Maybe Text
      -- ^ PlatformApplicationArn is returned.
    } deriving (Show, Generic)

-- | PlatformApplicationArn is returned.
cparPlatformApplicationArn :: Lens' CreatePlatformApplicationResponse (Maybe Text)
cparPlatformApplicationArn f x =
    f (_cparPlatformApplicationArn x)
        <&> \y -> x { _cparPlatformApplicationArn = y }
{-# INLINE cparPlatformApplicationArn #-}

instance FromXML CreatePlatformApplicationResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreatePlatformApplication where
    type Sv CreatePlatformApplication = SNS
    type Rs CreatePlatformApplication = CreatePlatformApplicationResponse

    request = post "CreatePlatformApplication"
    response _ = xmlResponse
