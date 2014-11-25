{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.IAM.DeactivateMFADevice
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deactivates the specified MFA device and removes it from association with the
-- user name for which it was originally enabled.
--
-- For more information about creating and working with virtual MFA devices, go
-- to <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_VirtualMFA.html Using a Virtual MFA Device> in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeactivateMFADevice.html>
module Network.AWS.IAM.DeactivateMFADevice
    (
    -- * Request
      DeactivateMFADevice
    -- ** Request constructor
    , deactivateMFADevice
    -- ** Request lenses
    , dmfadSerialNumber
    , dmfadUserName

    -- * Response
    , DeactivateMFADeviceResponse
    -- ** Response constructor
    , deactivateMFADeviceResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.IAM.Types
import qualified GHC.Exts

data DeactivateMFADevice = DeactivateMFADevice
    { _dmfadSerialNumber :: Text
    , _dmfadUserName     :: Text
    } deriving (Eq, Ord, Show)

-- | 'DeactivateMFADevice' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmfadSerialNumber' @::@ 'Text'
--
-- * 'dmfadUserName' @::@ 'Text'
--
deactivateMFADevice :: Text -- ^ 'dmfadUserName'
                    -> Text -- ^ 'dmfadSerialNumber'
                    -> DeactivateMFADevice
deactivateMFADevice p1 p2 = DeactivateMFADevice
    { _dmfadUserName     = p1
    , _dmfadSerialNumber = p2
    }

-- | The serial number that uniquely identifies the MFA device. For virtual MFA
-- devices, the serial number is the device ARN.
dmfadSerialNumber :: Lens' DeactivateMFADevice Text
dmfadSerialNumber =
    lens _dmfadSerialNumber (\s a -> s { _dmfadSerialNumber = a })

-- | The name of the user whose MFA device you want to deactivate.
dmfadUserName :: Lens' DeactivateMFADevice Text
dmfadUserName = lens _dmfadUserName (\s a -> s { _dmfadUserName = a })

data DeactivateMFADeviceResponse = DeactivateMFADeviceResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeactivateMFADeviceResponse' constructor.
deactivateMFADeviceResponse :: DeactivateMFADeviceResponse
deactivateMFADeviceResponse = DeactivateMFADeviceResponse

instance ToPath DeactivateMFADevice where
    toPath = const "/"

instance ToQuery DeactivateMFADevice where
    toQuery DeactivateMFADevice{..} = mconcat
        [ "SerialNumber" =? _dmfadSerialNumber
        , "UserName"     =? _dmfadUserName
        ]

instance ToHeaders DeactivateMFADevice

instance AWSRequest DeactivateMFADevice where
    type Sv DeactivateMFADevice = IAM
    type Rs DeactivateMFADevice = DeactivateMFADeviceResponse

    request  = post "DeactivateMFADevice"
    response = nullResponse DeactivateMFADeviceResponse
