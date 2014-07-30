{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

-- Module      : Network.AWS.IAM.V2010_05_08.DeleteVirtualMFADevice
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
module Network.AWS.IAM.V2010_05_08.DeleteVirtualMFADevice where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

data DeleteVirtualMFADevice = DeleteVirtualMFADevice
    { _dvmfadrSerialNumber :: Text
      -- ^ The serial number that uniquely identifies the MFA device. For
      -- virtual MFA devices, the serial number is the same as the ARN.
    } deriving (Generic)

instance ToQuery DeleteVirtualMFADevice where
    toQuery = genericToQuery def

instance AWSRequest DeleteVirtualMFADevice where
    type Sv DeleteVirtualMFADevice = IAM
    type Rs DeleteVirtualMFADevice = DeleteVirtualMFADeviceResponse

    request = post "DeleteVirtualMFADevice"
    response _ _ = return (Right DeleteVirtualMFADeviceResponse)

data DeleteVirtualMFADeviceResponse = DeleteVirtualMFADeviceResponse
    deriving (Eq, Show, Generic)
