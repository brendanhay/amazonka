{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.ResyncMFADevice
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
module Network.AWS.IAM.ResyncMFADevice
    (
    -- * Request
      ResyncMFADevice
    -- ** Request constructor
    , mkResyncMFADevice
    -- ** Request lenses
    , rmfadUserName
    , rmfadSerialNumber
    , rmfadAuthenticationCode1
    , rmfadAuthenticationCode2

    -- * Response
    , ResyncMFADeviceResponse
    -- ** Response constructor
    , mkResyncMFADeviceResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

data ResyncMFADevice = ResyncMFADevice
    { _rmfadUserName :: !Text
    , _rmfadSerialNumber :: !Text
    , _rmfadAuthenticationCode1 :: !Text
    , _rmfadAuthenticationCode2 :: !Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ResyncMFADevice' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @UserName ::@ @Text@
--
-- * @SerialNumber ::@ @Text@
--
-- * @AuthenticationCode1 ::@ @Text@
--
-- * @AuthenticationCode2 ::@ @Text@
--
mkResyncMFADevice :: Text -- ^ 'rmfadUserName'
                  -> Text -- ^ 'rmfadSerialNumber'
                  -> Text -- ^ 'rmfadAuthenticationCode1'
                  -> Text -- ^ 'rmfadAuthenticationCode2'
                  -> ResyncMFADevice
mkResyncMFADevice p1 p2 p3 p4 = ResyncMFADevice
    { _rmfadUserName = p1
    , _rmfadSerialNumber = p2
    , _rmfadAuthenticationCode1 = p3
    , _rmfadAuthenticationCode2 = p4
    }

-- | Name of the user whose MFA device you want to resynchronize.
rmfadUserName :: Lens' ResyncMFADevice Text
rmfadUserName = lens _rmfadUserName (\s a -> s { _rmfadUserName = a })

-- | Serial number that uniquely identifies the MFA device.
rmfadSerialNumber :: Lens' ResyncMFADevice Text
rmfadSerialNumber =
    lens _rmfadSerialNumber (\s a -> s { _rmfadSerialNumber = a })

-- | An authentication code emitted by the device.
rmfadAuthenticationCode1 :: Lens' ResyncMFADevice Text
rmfadAuthenticationCode1 =
    lens _rmfadAuthenticationCode1
         (\s a -> s { _rmfadAuthenticationCode1 = a })

-- | A subsequent authentication code emitted by the device.
rmfadAuthenticationCode2 :: Lens' ResyncMFADevice Text
rmfadAuthenticationCode2 =
    lens _rmfadAuthenticationCode2
         (\s a -> s { _rmfadAuthenticationCode2 = a })

instance ToQuery ResyncMFADevice where
    toQuery = genericQuery def

data ResyncMFADeviceResponse = ResyncMFADeviceResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ResyncMFADeviceResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkResyncMFADeviceResponse :: ResyncMFADeviceResponse
mkResyncMFADeviceResponse = ResyncMFADeviceResponse

instance AWSRequest ResyncMFADevice where
    type Sv ResyncMFADevice = IAM
    type Rs ResyncMFADevice = ResyncMFADeviceResponse

    request = post "ResyncMFADevice"
    response _ = nullaryResponse ResyncMFADeviceResponse
