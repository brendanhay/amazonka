{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.CreateVirtualMFADevice
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new virtual MFA device for the AWS account. After creating the
-- virtual MFA, use EnableMFADevice to attach the MFA device to an IAM user.
-- For more information about creating and working with virtual MFA devices,
-- go to Using a Virtual MFA Device in the Using IAM guide. For information
-- about limits on the number of MFA devices you can create, see Limitations
-- on Entities in the Using IAM guide. The seed information contained in the
-- QR code and the Base32 string should be treated like any other secret
-- access information, such as your AWS access keys or your passwords. After
-- you provision your virtual device, you should ensure that the information
-- is destroyed following secure procedures. https://iam.amazonaws.com/
-- ?Action=CreateVirtualMFADevice &VirtualMFADeviceName=ExampleName &Path=/
-- &Version=2010-05-08 &AUTHPARAMS arn:aws:iam::123456789012:mfa/ExampleName
-- 2K5K5XTLA7GGE75TQLYEXAMPLEEXAMPLEEXAMPLECHDFW4KJYZ6 UFQ75LL7COCYKM
-- 89504E470D0A1A0AASDFAHSDFKJKLJFKALSDFJASDF
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.CreateVirtualMFADevice
    (
    -- * Request
      CreateVirtualMFADevice
    -- ** Request constructor
    , createVirtualMFADevice
    -- ** Request lenses
    , cvmfadPath
    , cvmfadVirtualMFADeviceName

    -- * Response
    , CreateVirtualMFADeviceResponse
    -- ** Response constructor
    , createVirtualMFADeviceResponse
    -- ** Response lenses
    , cvmfadrVirtualMFADevice
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

data CreateVirtualMFADevice = CreateVirtualMFADevice
    { _cvmfadPath :: Maybe Text
    , _cvmfadVirtualMFADeviceName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateVirtualMFADevice' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Path ::@ @Maybe Text@
--
-- * @VirtualMFADeviceName ::@ @Text@
--
createVirtualMFADevice :: Text -- ^ 'cvmfadVirtualMFADeviceName'
                         -> CreateVirtualMFADevice
createVirtualMFADevice p2 = CreateVirtualMFADevice
    { _cvmfadPath = Nothing
    , _cvmfadVirtualMFADeviceName = p2
    }

-- | The path for the virtual MFA device. For more information about paths, see
-- Identifiers for IAM Entities in the Using IAM guide. This parameter is
-- optional. If it is not included, it defaults to a slash (/).
cvmfadPath :: Lens' CreateVirtualMFADevice (Maybe Text)
cvmfadPath = lens _cvmfadPath (\s a -> s { _cvmfadPath = a })

-- | The name of the virtual MFA device. Use with path to uniquely identify a
-- virtual MFA device.
cvmfadVirtualMFADeviceName :: Lens' CreateVirtualMFADevice Text
cvmfadVirtualMFADeviceName =
    lens _cvmfadVirtualMFADeviceName
         (\s a -> s { _cvmfadVirtualMFADeviceName = a })

instance ToQuery CreateVirtualMFADevice where
    toQuery = genericQuery def

-- | Contains the result of a successful invocation of the
-- CreateVirtualMFADevice action.
newtype CreateVirtualMFADeviceResponse = CreateVirtualMFADeviceResponse
    { _cvmfadrVirtualMFADevice :: VirtualMFADevice
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateVirtualMFADeviceResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VirtualMFADevice ::@ @VirtualMFADevice@
--
createVirtualMFADeviceResponse :: VirtualMFADevice -- ^ 'cvmfadrVirtualMFADevice'
                                 -> CreateVirtualMFADeviceResponse
createVirtualMFADeviceResponse p1 = CreateVirtualMFADeviceResponse
    { _cvmfadrVirtualMFADevice = p1
    }

-- | A newly created virtual MFA device.
cvmfadrVirtualMFADevice :: Lens' CreateVirtualMFADeviceResponse VirtualMFADevice
cvmfadrVirtualMFADevice =
    lens _cvmfadrVirtualMFADevice
         (\s a -> s { _cvmfadrVirtualMFADevice = a })

instance FromXML CreateVirtualMFADeviceResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateVirtualMFADevice where
    type Sv CreateVirtualMFADevice = IAM
    type Rs CreateVirtualMFADevice = CreateVirtualMFADeviceResponse

    request = post "CreateVirtualMFADevice"
    response _ = xmlResponse
