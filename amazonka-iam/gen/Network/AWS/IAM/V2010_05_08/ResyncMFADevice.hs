{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.ResyncMFADevice
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Synchronizes the specified MFA device with AWS servers.
-- https://iam.amazonaws.com/ ?Action=ResyncMFADevice &UserName=Bob
-- &SerialNumber=R1234 &AuthenticationCode1=234567 &AuthenticationCode2=987654
-- &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.V2010_05_08.ResyncMFADevice
    (
    -- * Request
      ResyncMFADevice
    -- ** Request constructor
    , mkResyncMFADeviceRequest
    -- ** Request lenses
    , rmfadrUserName
    , rmfadrSerialNumber
    , rmfadrAuthenticationCode1
    , rmfadrAuthenticationCode2

    -- * Response
    , ResyncMFADeviceResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ResyncMFADevice' request.
mkResyncMFADeviceRequest :: Text -- ^ 'rmfadrUserName'
                         -> Text -- ^ 'rmfadrSerialNumber'
                         -> Text -- ^ 'rmfadrAuthenticationCode1'
                         -> Text -- ^ 'rmfadrAuthenticationCode2'
                         -> ResyncMFADevice
mkResyncMFADeviceRequest p1 p2 p3 p4 = ResyncMFADevice
    { _rmfadrUserName = p1
    , _rmfadrSerialNumber = p2
    , _rmfadrAuthenticationCode1 = p3
    , _rmfadrAuthenticationCode2 = p4
    }
{-# INLINE mkResyncMFADeviceRequest #-}

data ResyncMFADevice = ResyncMFADevice
    { _rmfadrUserName :: Text
      -- ^ Name of the user whose MFA device you want to resynchronize.
    , _rmfadrSerialNumber :: Text
      -- ^ Serial number that uniquely identifies the MFA device.
    , _rmfadrAuthenticationCode1 :: Text
      -- ^ An authentication code emitted by the device.
    , _rmfadrAuthenticationCode2 :: Text
      -- ^ A subsequent authentication code emitted by the device.
    } deriving (Show, Generic)

-- | Name of the user whose MFA device you want to resynchronize.
rmfadrUserName :: Lens' ResyncMFADevice (Text)
rmfadrUserName = lens _rmfadrUserName (\s a -> s { _rmfadrUserName = a })
{-# INLINE rmfadrUserName #-}

-- | Serial number that uniquely identifies the MFA device.
rmfadrSerialNumber :: Lens' ResyncMFADevice (Text)
rmfadrSerialNumber = lens _rmfadrSerialNumber (\s a -> s { _rmfadrSerialNumber = a })
{-# INLINE rmfadrSerialNumber #-}

-- | An authentication code emitted by the device.
rmfadrAuthenticationCode1 :: Lens' ResyncMFADevice (Text)
rmfadrAuthenticationCode1 = lens _rmfadrAuthenticationCode1 (\s a -> s { _rmfadrAuthenticationCode1 = a })
{-# INLINE rmfadrAuthenticationCode1 #-}

-- | A subsequent authentication code emitted by the device.
rmfadrAuthenticationCode2 :: Lens' ResyncMFADevice (Text)
rmfadrAuthenticationCode2 = lens _rmfadrAuthenticationCode2 (\s a -> s { _rmfadrAuthenticationCode2 = a })
{-# INLINE rmfadrAuthenticationCode2 #-}

instance ToQuery ResyncMFADevice where
    toQuery = genericQuery def

data ResyncMFADeviceResponse = ResyncMFADeviceResponse
    deriving (Eq, Show, Generic)

instance AWSRequest ResyncMFADevice where
    type Sv ResyncMFADevice = IAM
    type Rs ResyncMFADevice = ResyncMFADeviceResponse

    request = post "ResyncMFADevice"
    response _ = nullaryResponse ResyncMFADeviceResponse
