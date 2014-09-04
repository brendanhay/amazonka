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
    , mkDeactivateMFADeviceRequest
    -- ** Request lenses
    , dmfadrUserName
    , dmfadrSerialNumber

    -- * Response
    , DeactivateMFADeviceResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.IAM.V2010_05_08.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeactivateMFADevice' request.
mkDeactivateMFADeviceRequest :: Text -- ^ 'dmfadrUserName'
                             -> Text -- ^ 'dmfadrSerialNumber'
                             -> DeactivateMFADevice
mkDeactivateMFADeviceRequest p1 p2 = DeactivateMFADevice
    { _dmfadrUserName = p1
    , _dmfadrSerialNumber = p2
    }
{-# INLINE mkDeactivateMFADeviceRequest #-}

data DeactivateMFADevice = DeactivateMFADevice
    { _dmfadrUserName :: Text
      -- ^ Name of the user whose MFA device you want to deactivate.
    , _dmfadrSerialNumber :: Text
      -- ^ The serial number that uniquely identifies the MFA device. For
      -- virtual MFA devices, the serial number is the device ARN.
    } deriving (Show, Generic)

-- | Name of the user whose MFA device you want to deactivate.
dmfadrUserName :: Lens' DeactivateMFADevice (Text)
dmfadrUserName = lens _dmfadrUserName (\s a -> s { _dmfadrUserName = a })
{-# INLINE dmfadrUserName #-}

-- | The serial number that uniquely identifies the MFA device. For virtual MFA
-- devices, the serial number is the device ARN.
dmfadrSerialNumber :: Lens' DeactivateMFADevice (Text)
dmfadrSerialNumber = lens _dmfadrSerialNumber (\s a -> s { _dmfadrSerialNumber = a })
{-# INLINE dmfadrSerialNumber #-}

instance ToQuery DeactivateMFADevice where
    toQuery = genericQuery def

data DeactivateMFADeviceResponse = DeactivateMFADeviceResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeactivateMFADevice where
    type Sv DeactivateMFADevice = IAM
    type Rs DeactivateMFADevice = DeactivateMFADeviceResponse

    request = post "DeactivateMFADevice"
    response _ = nullaryResponse DeactivateMFADeviceResponse
