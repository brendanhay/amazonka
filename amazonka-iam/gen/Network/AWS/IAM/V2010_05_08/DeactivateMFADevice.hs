{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.DeactivateMFADevice
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deactivates the specified MFA device and removes it from association with
-- the user name for which it was originally enabled.
-- https://iam.amazonaws.com/ ?Action=DeactivateMFADevice &UserName=Bob
-- &SerialNumber=R1234 &AUTHPARAMS 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.V2010_05_08.DeactivateMFADevice
    (
    -- * Request
      DeactivateMFADevice
    -- ** Request constructor
    , mkDeactivateMFADevice
    -- ** Request lenses
    , dmfadUserName
    , dmfadSerialNumber

    -- * Response
    , DeactivateMFADeviceResponse
    -- ** Response constructor
    , mkDeactivateMFADeviceResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

data DeactivateMFADevice = DeactivateMFADevice
    { _dmfadUserName :: Text
    , _dmfadSerialNumber :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeactivateMFADevice' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @UserName ::@ @Text@
--
-- * @SerialNumber ::@ @Text@
--
mkDeactivateMFADevice :: Text -- ^ 'dmfadUserName'
                      -> Text -- ^ 'dmfadSerialNumber'
                      -> DeactivateMFADevice
mkDeactivateMFADevice p1 p2 = DeactivateMFADevice
    { _dmfadUserName = p1
    , _dmfadSerialNumber = p2
    }

-- | Name of the user whose MFA device you want to deactivate.
dmfadUserName :: Lens' DeactivateMFADevice Text
dmfadUserName = lens _dmfadUserName (\s a -> s { _dmfadUserName = a })

-- | The serial number that uniquely identifies the MFA device. For virtual MFA
-- devices, the serial number is the device ARN.
dmfadSerialNumber :: Lens' DeactivateMFADevice Text
dmfadSerialNumber =
    lens _dmfadSerialNumber (\s a -> s { _dmfadSerialNumber = a })

instance ToQuery DeactivateMFADevice where
    toQuery = genericQuery def

data DeactivateMFADeviceResponse = DeactivateMFADeviceResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeactivateMFADeviceResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDeactivateMFADeviceResponse :: DeactivateMFADeviceResponse
mkDeactivateMFADeviceResponse = DeactivateMFADeviceResponse

instance AWSRequest DeactivateMFADevice where
    type Sv DeactivateMFADevice = IAM
    type Rs DeactivateMFADevice = DeactivateMFADeviceResponse

    request = post "DeactivateMFADevice"
    response _ = nullaryResponse DeactivateMFADeviceResponse
