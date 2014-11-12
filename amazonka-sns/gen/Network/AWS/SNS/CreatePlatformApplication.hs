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
module Network.AWS.SNS.CreatePlatformApplication
    (
    -- * Request
      CreatePlatformApplicationInput
    -- ** Request constructor
    , createPlatformApplication
    -- ** Request lenses
    , cpaiAttributes
    , cpaiName
    , cpaiPlatform

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

data CreatePlatformApplicationInput = CreatePlatformApplicationInput
    { _cpaiAttributes :: Map Text Text
    , _cpaiName       :: Text
    , _cpaiPlatform   :: Text
    } deriving (Eq, Show, Generic)

-- | 'CreatePlatformApplicationInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpaiAttributes' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'cpaiName' @::@ 'Text'
--
-- * 'cpaiPlatform' @::@ 'Text'
--
createPlatformApplication :: Text -- ^ 'cpaiName'
                          -> Text -- ^ 'cpaiPlatform'
                          -> CreatePlatformApplicationInput
createPlatformApplication p1 p2 = CreatePlatformApplicationInput
    { _cpaiName       = p1
    , _cpaiPlatform   = p2
    , _cpaiAttributes = mempty
    }

-- | For a list of attributes, see SetPlatformApplicationAttributes.
cpaiAttributes :: Lens' CreatePlatformApplicationInput (HashMap Text Text)
cpaiAttributes = lens _cpaiAttributes (\s a -> s { _cpaiAttributes = a })
    . _Map

-- | Application names must be made up of only uppercase and lowercase ASCII
-- letters, numbers, underscores, hyphens, and periods, and must be between
-- 1 and 256 characters long.
cpaiName :: Lens' CreatePlatformApplicationInput Text
cpaiName = lens _cpaiName (\s a -> s { _cpaiName = a })

-- | The following platforms are supported: ADM (Amazon Device Messaging),
-- APNS (Apple Push Notification Service), APNS_SANDBOX, and GCM (Google
-- Cloud Messaging).
cpaiPlatform :: Lens' CreatePlatformApplicationInput Text
cpaiPlatform = lens _cpaiPlatform (\s a -> s { _cpaiPlatform = a })

instance ToQuery CreatePlatformApplicationInput

instance ToPath CreatePlatformApplicationInput where
    toPath = const "/"

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

instance FromXML CreatePlatformApplicationResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreatePlatformApplicationResponse"

instance AWSRequest CreatePlatformApplicationInput where
    type Sv CreatePlatformApplicationInput = SNS
    type Rs CreatePlatformApplicationInput = CreatePlatformApplicationResponse

    request  = post "CreatePlatformApplication"
    response = xmlResponse $ \h x -> CreatePlatformApplicationResponse
        <$> x %| "PlatformApplicationArn"
