{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
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
-- is destroyed following secure procedures.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateVirtualMFADevice.html>
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

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data CreateVirtualMFADevice = CreateVirtualMFADevice
    { _cvmfadPath                 :: Maybe Text
    , _cvmfadVirtualMFADeviceName :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateVirtualMFADevice' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvmfadPath' @::@ 'Maybe' 'Text'
--
-- * 'cvmfadVirtualMFADeviceName' @::@ 'Text'
--
createVirtualMFADevice :: Text -- ^ 'cvmfadVirtualMFADeviceName'
                       -> CreateVirtualMFADevice
createVirtualMFADevice p1 = CreateVirtualMFADevice
    { _cvmfadVirtualMFADeviceName = p1
    , _cvmfadPath                 = Nothing
    }

-- | The path for the virtual MFA device. For more information about paths,
-- see IAM Identifiers in the Using IAM guide. This parameter is optional.
-- If it is not included, it defaults to a slash (/).
cvmfadPath :: Lens' CreateVirtualMFADevice (Maybe Text)
cvmfadPath = lens _cvmfadPath (\s a -> s { _cvmfadPath = a })

-- | The name of the virtual MFA device. Use with path to uniquely identify a
-- virtual MFA device.
cvmfadVirtualMFADeviceName :: Lens' CreateVirtualMFADevice Text
cvmfadVirtualMFADeviceName =
    lens _cvmfadVirtualMFADeviceName
        (\s a -> s { _cvmfadVirtualMFADeviceName = a })

newtype CreateVirtualMFADeviceResponse = CreateVirtualMFADeviceResponse
    { _cvmfadrVirtualMFADevice :: VirtualMFADevice
    } deriving (Eq, Show, Generic)

-- | 'CreateVirtualMFADeviceResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvmfadrVirtualMFADevice' @::@ 'VirtualMFADevice'
--
createVirtualMFADeviceResponse :: VirtualMFADevice -- ^ 'cvmfadrVirtualMFADevice'
                               -> CreateVirtualMFADeviceResponse
createVirtualMFADeviceResponse p1 = CreateVirtualMFADeviceResponse
    { _cvmfadrVirtualMFADevice = p1
    }

-- | A newly created virtual MFA device.
cvmfadrVirtualMFADevice :: Lens' CreateVirtualMFADeviceResponse VirtualMFADevice
cvmfadrVirtualMFADevice =
    lens _cvmfadrVirtualMFADevice (\s a -> s { _cvmfadrVirtualMFADevice = a })

instance ToPath CreateVirtualMFADevice where
    toPath = const "/"

instance ToQuery CreateVirtualMFADevice

instance ToHeaders CreateVirtualMFADevice

instance AWSRequest CreateVirtualMFADevice where
    type Sv CreateVirtualMFADevice = IAM
    type Rs CreateVirtualMFADevice = CreateVirtualMFADeviceResponse

    request  = post "CreateVirtualMFADevice"
    response = xmlResponse

instance FromXML CreateVirtualMFADeviceResponse where
    parseXML = withElement "CreateVirtualMFADeviceResult" $ \x ->
            <$> x .@ "VirtualMFADevice"
