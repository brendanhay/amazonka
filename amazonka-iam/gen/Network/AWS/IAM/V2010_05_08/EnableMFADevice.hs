{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.EnableMFADevice
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
-- by the user name associated with the device. https://iam.amazonaws.com/
-- ?Action=EnableMFADevice &UserName=Bob &SerialNumber=R1234
-- &AuthenticationCode1=234567 &AuthenticationCode2=987654 &AUTHPARAMS
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.V2010_05_08.EnableMFADevice
    (
    -- * Request
      EnableMFADevice
    -- ** Request constructor
    , mkEnableMFADevice
    -- ** Request lenses
    , emfadUserName
    , emfadSerialNumber
    , emfadAuthenticationCode1
    , emfadAuthenticationCode2

    -- * Response
    , EnableMFADeviceResponse
    -- ** Response constructor
    , mkEnableMFADeviceResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | 
data EnableMFADevice = EnableMFADevice
    { _emfadUserName :: Text
    , _emfadSerialNumber :: Text
    , _emfadAuthenticationCode1 :: Text
    , _emfadAuthenticationCode2 :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'EnableMFADevice' request.
mkEnableMFADevice :: Text -- ^ 'emfadUserName'
                  -> Text -- ^ 'emfadSerialNumber'
                  -> Text -- ^ 'emfadAuthenticationCode1'
                  -> Text -- ^ 'emfadAuthenticationCode2'
                  -> EnableMFADevice
mkEnableMFADevice p1 p2 p3 p4 = EnableMFADevice
    { _emfadUserName = p1
    , _emfadSerialNumber = p2
    , _emfadAuthenticationCode1 = p3
    , _emfadAuthenticationCode2 = p4
    }

-- | Name of the user for whom you want to enable the MFA device.
emfadUserName :: Lens' EnableMFADevice Text
emfadUserName = lens _emfadUserName (\s a -> s { _emfadUserName = a })

-- | The serial number that uniquely identifies the MFA device. For virtual MFA
-- devices, the serial number is the device ARN.
emfadSerialNumber :: Lens' EnableMFADevice Text
emfadSerialNumber =
    lens _emfadSerialNumber (\s a -> s { _emfadSerialNumber = a })

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

instance ToQuery EnableMFADevice where
    toQuery = genericQuery def

data EnableMFADeviceResponse = EnableMFADeviceResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'EnableMFADeviceResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkEnableMFADeviceResponse :: EnableMFADeviceResponse
mkEnableMFADeviceResponse = EnableMFADeviceResponse

instance AWSRequest EnableMFADevice where
    type Sv EnableMFADevice = IAM
    type Rs EnableMFADevice = EnableMFADeviceResponse

    request = post "EnableMFADevice"
    response _ = nullaryResponse EnableMFADeviceResponse
