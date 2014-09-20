{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.DeleteVirtualMFADevice
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a virtual MFA device. You must deactivate a user's virtual MFA
-- device before you can delete it. For information about deactivating MFA
-- devices, see DeactivateMFADevice. https://iam.amazonaws.com/
-- ?Action=DeleteVirtualMFADevice
-- &SerialNumber=arn:aws:iam::123456789012:mfa/ExampleName &Version=2010-05-08
-- &AUTHPARAMS arn:aws:iam::123456789012:mfa/ExampleName
-- 7a62c49f-347e-4fc4-9331-6e8eEXAMPLE.
module Network.AWS.IAM.DeleteVirtualMFADevice
    (
    -- * Request
      DeleteVirtualMFADevice
    -- ** Request constructor
    , deleteVirtualMFADevice
    -- ** Request lenses
    , dvmfadSerialNumber

    -- * Response
    , DeleteVirtualMFADeviceResponse
    -- ** Response constructor
    , deleteVirtualMFADeviceResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import Network.AWS.Prelude

newtype DeleteVirtualMFADevice = DeleteVirtualMFADevice
    { _dvmfadSerialNumber :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteVirtualMFADevice' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SerialNumber ::@ @Text@
--
deleteVirtualMFADevice :: Text -- ^ 'dvmfadSerialNumber'
                       -> DeleteVirtualMFADevice
deleteVirtualMFADevice p1 = DeleteVirtualMFADevice
    { _dvmfadSerialNumber = p1
    }

-- | The serial number that uniquely identifies the MFA device. For virtual MFA
-- devices, the serial number is the same as the ARN.
dvmfadSerialNumber :: Lens' DeleteVirtualMFADevice Text
dvmfadSerialNumber =
    lens _dvmfadSerialNumber (\s a -> s { _dvmfadSerialNumber = a })

instance ToQuery DeleteVirtualMFADevice where
    toQuery = genericQuery def

data DeleteVirtualMFADeviceResponse = DeleteVirtualMFADeviceResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteVirtualMFADeviceResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
deleteVirtualMFADeviceResponse :: DeleteVirtualMFADeviceResponse
deleteVirtualMFADeviceResponse = DeleteVirtualMFADeviceResponse

instance AWSRequest DeleteVirtualMFADevice where
    type Sv DeleteVirtualMFADevice = IAM
    type Rs DeleteVirtualMFADevice = DeleteVirtualMFADeviceResponse

    request = post "DeleteVirtualMFADevice"
    response _ = nullaryResponse DeleteVirtualMFADeviceResponse
