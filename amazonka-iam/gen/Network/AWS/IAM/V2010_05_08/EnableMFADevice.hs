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
    , enableMFADevice
    -- ** Request lenses
    , emfadrAuthenticationCode1
    , emfadrAuthenticationCode2
    , emfadrUserName
    , emfadrSerialNumber

    -- * Response
    , EnableMFADeviceResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'EnableMFADevice' request.
enableMFADevice :: Text -- ^ 'emfadrAuthenticationCode1'
                -> Text -- ^ 'emfadrAuthenticationCode2'
                -> Text -- ^ 'emfadrUserName'
                -> Text -- ^ 'emfadrSerialNumber'
                -> EnableMFADevice
enableMFADevice p1 p2 p3 p4 = EnableMFADevice
    { _emfadrAuthenticationCode1 = p1
    , _emfadrAuthenticationCode2 = p2
    , _emfadrUserName = p3
    , _emfadrSerialNumber = p4
    }
{-# INLINE enableMFADevice #-}

data EnableMFADevice = EnableMFADevice
    { _emfadrAuthenticationCode1 :: Text
      -- ^ An authentication code emitted by the device.
    , _emfadrAuthenticationCode2 :: Text
      -- ^ A subsequent authentication code emitted by the device.
    , _emfadrUserName :: Text
      -- ^ Name of the user for whom you want to enable the MFA device.
    , _emfadrSerialNumber :: Text
      -- ^ The serial number that uniquely identifies the MFA device. For
      -- virtual MFA devices, the serial number is the device ARN.
    } deriving (Show, Generic)

-- | An authentication code emitted by the device.
emfadrAuthenticationCode1 :: Lens' EnableMFADevice (Text)
emfadrAuthenticationCode1 f x =
    f (_emfadrAuthenticationCode1 x)
        <&> \y -> x { _emfadrAuthenticationCode1 = y }
{-# INLINE emfadrAuthenticationCode1 #-}

-- | A subsequent authentication code emitted by the device.
emfadrAuthenticationCode2 :: Lens' EnableMFADevice (Text)
emfadrAuthenticationCode2 f x =
    f (_emfadrAuthenticationCode2 x)
        <&> \y -> x { _emfadrAuthenticationCode2 = y }
{-# INLINE emfadrAuthenticationCode2 #-}

-- | Name of the user for whom you want to enable the MFA device.
emfadrUserName :: Lens' EnableMFADevice (Text)
emfadrUserName f x =
    f (_emfadrUserName x)
        <&> \y -> x { _emfadrUserName = y }
{-# INLINE emfadrUserName #-}

-- | The serial number that uniquely identifies the MFA device. For virtual MFA
-- devices, the serial number is the device ARN.
emfadrSerialNumber :: Lens' EnableMFADevice (Text)
emfadrSerialNumber f x =
    f (_emfadrSerialNumber x)
        <&> \y -> x { _emfadrSerialNumber = y }
{-# INLINE emfadrSerialNumber #-}

instance ToQuery EnableMFADevice where
    toQuery = genericQuery def

data EnableMFADeviceResponse = EnableMFADeviceResponse
    deriving (Eq, Show, Generic)

instance AWSRequest EnableMFADevice where
    type Sv EnableMFADevice = IAM
    type Rs EnableMFADevice = EnableMFADeviceResponse

    request = post "EnableMFADevice"
    response _ = nullaryResponse EnableMFADeviceResponse
