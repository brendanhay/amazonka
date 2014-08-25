{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.V2010_05_08.CreateVirtualMFADevice
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
module Network.AWS.IAM.V2010_05_08.CreateVirtualMFADevice where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateVirtualMFADevice' request.
createVirtualMFADevice :: Text -- ^ '_cvmfadrVirtualMFADeviceName'
                       -> CreateVirtualMFADevice
createVirtualMFADevice p1 = CreateVirtualMFADevice
    { _cvmfadrVirtualMFADeviceName = p1
    , _cvmfadrPath = Nothing
    }

data CreateVirtualMFADevice = CreateVirtualMFADevice
    { _cvmfadrVirtualMFADeviceName :: Text
      -- ^ The name of the virtual MFA device. Use with path to uniquely
      -- identify a virtual MFA device.
    , _cvmfadrPath :: Maybe Text
      -- ^ The path for the virtual MFA device. For more information about
      -- paths, see Identifiers for IAM Entities in the Using IAM guide.
      -- This parameter is optional. If it is not included, it defaults to
      -- a slash (/).
    } deriving (Show, Generic)

makeLenses ''CreateVirtualMFADevice

instance ToQuery CreateVirtualMFADevice where
    toQuery = genericQuery def

data CreateVirtualMFADeviceResponse = CreateVirtualMFADeviceResponse
    { _cvmfadsVirtualMFADevice :: VirtualMFADevice
      -- ^ A newly created virtual MFA device.
    } deriving (Show, Generic)

makeLenses ''CreateVirtualMFADeviceResponse

instance FromXML CreateVirtualMFADeviceResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateVirtualMFADevice where
    type Sv CreateVirtualMFADevice = IAM
    type Rs CreateVirtualMFADevice = CreateVirtualMFADeviceResponse

    request = post "CreateVirtualMFADevice"
    response _ = xmlResponse
