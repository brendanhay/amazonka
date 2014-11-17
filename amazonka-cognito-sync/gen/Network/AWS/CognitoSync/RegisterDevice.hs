{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CognitoSync.RegisterDevice
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Registers a device to receive push sync notifications.
--
-- <http://docs.aws.amazon.com/cognitosync/latest/APIReference/API_RegisterDevice.html>
module Network.AWS.CognitoSync.RegisterDevice
    (
    -- * Request
      RegisterDevice
    -- ** Request constructor
    , registerDevice
    -- ** Request lenses
    , rdIdentityId
    , rdIdentityPoolId
    , rdPlatform
    , rdToken

    -- * Response
    , RegisterDeviceResponse
    -- ** Response constructor
    , registerDeviceResponse
    -- ** Response lenses
    , rdrDeviceId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.RestJSON
import Network.AWS.CognitoSync.Types
import qualified GHC.Exts

data RegisterDevice = RegisterDevice
    { _rdIdentityId     :: Text
    , _rdIdentityPoolId :: Text
    , _rdPlatform       :: Text
    , _rdToken          :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'RegisterDevice' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdIdentityId' @::@ 'Text'
--
-- * 'rdIdentityPoolId' @::@ 'Text'
--
-- * 'rdPlatform' @::@ 'Text'
--
-- * 'rdToken' @::@ 'Text'
--
registerDevice :: Text -- ^ 'rdIdentityPoolId'
               -> Text -- ^ 'rdIdentityId'
               -> Text -- ^ 'rdPlatform'
               -> Text -- ^ 'rdToken'
               -> RegisterDevice
registerDevice p1 p2 p3 p4 = RegisterDevice
    { _rdIdentityPoolId = p1
    , _rdIdentityId     = p2
    , _rdPlatform       = p3
    , _rdToken          = p4
    }

-- | The unique ID for this identity.
rdIdentityId :: Lens' RegisterDevice Text
rdIdentityId = lens _rdIdentityId (\s a -> s { _rdIdentityId = a })

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. Here, the ID of the pool that the identity belongs to.
rdIdentityPoolId :: Lens' RegisterDevice Text
rdIdentityPoolId = lens _rdIdentityPoolId (\s a -> s { _rdIdentityPoolId = a })

-- | The SNS platform type (e.g. GCM, SDM, APNS, APNS_SANDBOX).
rdPlatform :: Lens' RegisterDevice Text
rdPlatform = lens _rdPlatform (\s a -> s { _rdPlatform = a })

-- | The push token.
rdToken :: Lens' RegisterDevice Text
rdToken = lens _rdToken (\s a -> s { _rdToken = a })

newtype RegisterDeviceResponse = RegisterDeviceResponse
    { _rdrDeviceId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'RegisterDeviceResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rdrDeviceId' @::@ 'Maybe' 'Text'
--
registerDeviceResponse :: RegisterDeviceResponse
registerDeviceResponse = RegisterDeviceResponse
    { _rdrDeviceId = Nothing
    }

-- | The unique ID generated for this device by Cognito.
rdrDeviceId :: Lens' RegisterDeviceResponse (Maybe Text)
rdrDeviceId = lens _rdrDeviceId (\s a -> s { _rdrDeviceId = a })

instance ToPath RegisterDevice where
    toPath RegisterDevice{..} = mconcat
        [ "/identitypools/"
        , toText _rdIdentityPoolId
        , "/identity/"
        , toText _rdIdentityId
        , "/device"
        ]

instance ToQuery RegisterDevice where
    toQuery = const mempty

instance ToHeaders RegisterDevice

instance ToJSON RegisterDevice where
    toJSON RegisterDevice{..} = object
        [ "Platform" .= _rdPlatform
        , "Token"    .= _rdToken
        ]

instance AWSRequest RegisterDevice where
    type Sv RegisterDevice = CognitoSync
    type Rs RegisterDevice = RegisterDeviceResponse

    request  = post
    response = jsonResponse

instance FromJSON RegisterDeviceResponse where
    parseJSON = withObject "RegisterDeviceResponse" $ \o -> RegisterDeviceResponse
        <$> o .: "DeviceId"
