{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.IAM.DeactivateMFADevice
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deactivates the specified MFA device and removes it from association
-- with the user name for which it was originally enabled.
--
-- For more information about creating and working with virtual MFA
-- devices, go to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_VirtualMFA.html Using a Virtual MFA Device>
-- in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeactivateMFADevice.html>
module Network.AWS.IAM.DeactivateMFADevice
    (
    -- * Request
      DeactivateMFADevice
    -- ** Request constructor
    , deactivateMFADevice
    -- ** Request lenses
    , dmdUserName
    , dmdSerialNumber

    -- * Response
    , DeactivateMFADeviceResponse
    -- ** Response constructor
    , deactivateMFADeviceResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deactivateMFADevice' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmdUserName'
--
-- * 'dmdSerialNumber'
data DeactivateMFADevice = DeactivateMFADevice'
    { _dmdUserName     :: Text
    , _dmdSerialNumber :: Text
    } deriving (Eq,Read,Show)

-- | 'DeactivateMFADevice' smart constructor.
deactivateMFADevice :: Text -> Text -> DeactivateMFADevice
deactivateMFADevice pUserName pSerialNumber =
    DeactivateMFADevice'
    { _dmdUserName = pUserName
    , _dmdSerialNumber = pSerialNumber
    }

-- | The name of the user whose MFA device you want to deactivate.
dmdUserName :: Lens' DeactivateMFADevice Text
dmdUserName = lens _dmdUserName (\ s a -> s{_dmdUserName = a});

-- | The serial number that uniquely identifies the MFA device. For virtual
-- MFA devices, the serial number is the device ARN.
dmdSerialNumber :: Lens' DeactivateMFADevice Text
dmdSerialNumber = lens _dmdSerialNumber (\ s a -> s{_dmdSerialNumber = a});

instance AWSRequest DeactivateMFADevice where
        type Sv DeactivateMFADevice = IAM
        type Rs DeactivateMFADevice =
             DeactivateMFADeviceResponse
        request = post
        response = receiveNull DeactivateMFADeviceResponse'

instance ToHeaders DeactivateMFADevice where
        toHeaders = const mempty

instance ToPath DeactivateMFADevice where
        toPath = const "/"

instance ToQuery DeactivateMFADevice where
        toQuery DeactivateMFADevice'{..}
          = mconcat
              ["Action" =: ("DeactivateMFADevice" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "UserName" =: _dmdUserName,
               "SerialNumber" =: _dmdSerialNumber]

-- | /See:/ 'deactivateMFADeviceResponse' smart constructor.
data DeactivateMFADeviceResponse =
    DeactivateMFADeviceResponse'
    deriving (Eq,Read,Show)

-- | 'DeactivateMFADeviceResponse' smart constructor.
deactivateMFADeviceResponse :: DeactivateMFADeviceResponse
deactivateMFADeviceResponse = DeactivateMFADeviceResponse'
