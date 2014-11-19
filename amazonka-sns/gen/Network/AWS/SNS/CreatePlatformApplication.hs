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

-- Module      : Network.AWS.SNS.CreatePlatformApplication
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
-- Mobile Push Notifications.
--
-- <http://docs.aws.amazon.com/sns/latest/api/API_CreatePlatformApplication.html>
module Network.AWS.SNS.CreatePlatformApplication
    (
    -- * Request
      CreatePlatformApplication
    -- ** Request constructor
    , createPlatformApplication
    -- ** Request lenses
    , cpaAttributes
    , cpaName
    , cpaPlatform

    -- * Response
    , CreatePlatformApplicationResponse
    -- ** Response constructor
    , createPlatformApplicationResponse
    -- ** Response lenses
    , cparPlatformApplicationArn
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SNS.Types
import qualified GHC.Exts

data CreatePlatformApplication = CreatePlatformApplication
    { _cpaAttributes :: Map "entry" "key" Text "value" Text
    , _cpaName       :: Text
    , _cpaPlatform   :: Text
    } deriving (Eq, Show, Generic)

-- | 'CreatePlatformApplication' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpaAttributes' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'cpaName' @::@ 'Text'
--
-- * 'cpaPlatform' @::@ 'Text'
--
createPlatformApplication :: Text -- ^ 'cpaName'
                          -> Text -- ^ 'cpaPlatform'
                          -> CreatePlatformApplication
createPlatformApplication p1 p2 = CreatePlatformApplication
    { _cpaName       = p1
    , _cpaPlatform   = p2
    , _cpaAttributes = mempty
    }

-- | For a list of attributes, see SetPlatformApplicationAttributes.
cpaAttributes :: Lens' CreatePlatformApplication (HashMap Text Text)
cpaAttributes = lens _cpaAttributes (\s a -> s { _cpaAttributes = a })
    . _Map

-- | Application names must be made up of only uppercase and lowercase ASCII
-- letters, numbers, underscores, hyphens, and periods, and must be between
-- 1 and 256 characters long.
cpaName :: Lens' CreatePlatformApplication Text
cpaName = lens _cpaName (\s a -> s { _cpaName = a })

-- | The following platforms are supported: ADM (Amazon Device Messaging),
-- APNS (Apple Push Notification Service), APNS_SANDBOX, and GCM (Google
-- Cloud Messaging).
cpaPlatform :: Lens' CreatePlatformApplication Text
cpaPlatform = lens _cpaPlatform (\s a -> s { _cpaPlatform = a })

newtype CreatePlatformApplicationResponse = CreatePlatformApplicationResponse
    { _cparPlatformApplicationArn :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'CreatePlatformApplicationResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cparPlatformApplicationArn' @::@ 'Maybe' 'Text'
--
createPlatformApplicationResponse :: CreatePlatformApplicationResponse
createPlatformApplicationResponse = CreatePlatformApplicationResponse
    { _cparPlatformApplicationArn = Nothing
    }

-- | PlatformApplicationArn is returned.
cparPlatformApplicationArn :: Lens' CreatePlatformApplicationResponse (Maybe Text)
cparPlatformApplicationArn =
    lens _cparPlatformApplicationArn
        (\s a -> s { _cparPlatformApplicationArn = a })

instance ToPath CreatePlatformApplication where
    toPath = const "/"

instance ToQuery CreatePlatformApplication

instance ToHeaders CreatePlatformApplication

instance AWSRequest CreatePlatformApplication where
    type Sv CreatePlatformApplication = SNS
    type Rs CreatePlatformApplication = CreatePlatformApplicationResponse

    request  = post "CreatePlatformApplication"
    response = xmlResponse

instance FromXML CreatePlatformApplicationResponse where
    parseXML = withElement "CreatePlatformApplicationResult" $ \x ->
        <$> x .@? "PlatformApplicationArn"
