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

-- Module      : Network.AWS.IAM.EnableMFADevice
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Enables the specified MFA device and associates it with the specified user
-- name. When enabled, the MFA device is required for every subsequent login
-- by the user name associated with the device.
module Network.AWS.IAM.EnableMFADevice
    (
    -- * Request
      EnableMFADevice
    -- ** Request constructor
    , enableMFADevice
    -- ** Request lenses
    , emfadAuthenticationCode1
    , emfadAuthenticationCode2
    , emfadSerialNumber
    , emfadUserName

    -- * Response
    , EnableMFADeviceResponse
    -- ** Response constructor
    , enableMFADeviceResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types

data EnableMFADevice = EnableMFADevice
    { _emfadAuthenticationCode1 :: Text
    , _emfadAuthenticationCode2 :: Text
    , _emfadSerialNumber        :: Text
    , _emfadUserName            :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'EnableMFADevice' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'emfadAuthenticationCode1' @::@ 'Text'
--
-- * 'emfadAuthenticationCode2' @::@ 'Text'
--
-- * 'emfadSerialNumber' @::@ 'Text'
--
-- * 'emfadUserName' @::@ 'Text'
--
enableMFADevice :: Text -- ^ 'emfadUserName'
                -> Text -- ^ 'emfadSerialNumber'
                -> Text -- ^ 'emfadAuthenticationCode1'
                -> Text -- ^ 'emfadAuthenticationCode2'
                -> EnableMFADevice
enableMFADevice p1 p2 p3 p4 = EnableMFADevice
    { _emfadUserName            = p1
    , _emfadSerialNumber        = p2
    , _emfadAuthenticationCode1 = p3
    , _emfadAuthenticationCode2 = p4
    }

-- | An authentication code emitted by the device.
emfadAuthenticationCode1 :: Lens' EnableMFADevice Text
emfadAuthenticationCode1 =
    lens _emfadAuthenticationCode1
        (\s a -> s { _emfadAuthenticationCode1 = a })

-- | A subsequent authentication code emitted by the device.
emfadAuthenticationCode2 :: Lens' EnableMFADevice Text
emfadAuthenticationCode2 =
    lens _emfadAuthenticationCode2
        (\s a -> s { _emfadAuthenticationCode2 = a })

-- | The serial number that uniquely identifies the MFA device. For virtual
-- MFA devices, the serial number is the device ARN.
emfadSerialNumber :: Lens' EnableMFADevice Text
emfadSerialNumber =
    lens _emfadSerialNumber (\s a -> s { _emfadSerialNumber = a })

-- | The name of the user for whom you want to enable the MFA device.
emfadUserName :: Lens' EnableMFADevice Text
emfadUserName = lens _emfadUserName (\s a -> s { _emfadUserName = a })

instance ToQuery EnableMFADevice

instance ToPath EnableMFADevice where
    toPath = const "/"

data EnableMFADeviceResponse = EnableMFADeviceResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'EnableMFADeviceResponse' constructor.
enableMFADeviceResponse :: EnableMFADeviceResponse
enableMFADeviceResponse = EnableMFADeviceResponse

instance FromXML EnableMFADeviceResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EnableMFADeviceResponse"

instance AWSRequest EnableMFADevice where
    type Sv EnableMFADevice = IAM
    type Rs EnableMFADevice = EnableMFADeviceResponse

    request  = post "EnableMFADevice"
    response = nullaryResponse EnableMFADeviceResponse
