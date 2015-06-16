{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.IAM.CreateVirtualMFADevice
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

-- | Creates a new virtual MFA device for the AWS account. After creating the
-- virtual MFA, use EnableMFADevice to attach the MFA device to an IAM
-- user. For more information about creating and working with virtual MFA
-- devices, go to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_VirtualMFA.html Using a Virtual MFA Device>
-- in the /Using IAM/ guide.
--
-- For information about limits on the number of MFA devices you can
-- create, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html Limitations on Entities>
-- in the /Using IAM/ guide.
--
-- The seed information contained in the QR code and the Base32 string
-- should be treated like any other secret access information, such as your
-- AWS access keys or your passwords. After you provision your virtual
-- device, you should ensure that the information is destroyed following
-- secure procedures.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateVirtualMFADevice.html>
module Network.AWS.IAM.CreateVirtualMFADevice
    (
    -- * Request
      CreateVirtualMFADevice
    -- ** Request constructor
    , createVirtualMFADevice
    -- ** Request lenses
    , cvmdPath
    , cvmdVirtualMFADeviceName

    -- * Response
    , CreateVirtualMFADeviceResponse
    -- ** Response constructor
    , createVirtualMFADeviceResponse
    -- ** Response lenses
    , cvmdrVirtualMFADevice
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.IAM.Types

-- | /See:/ 'createVirtualMFADevice' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvmdPath'
--
-- * 'cvmdVirtualMFADeviceName'
data CreateVirtualMFADevice = CreateVirtualMFADevice'{_cvmdPath :: Maybe Text, _cvmdVirtualMFADeviceName :: Text} deriving (Eq, Read, Show)

-- | 'CreateVirtualMFADevice' smart constructor.
createVirtualMFADevice :: Text -> CreateVirtualMFADevice
createVirtualMFADevice pVirtualMFADeviceName = CreateVirtualMFADevice'{_cvmdPath = Nothing, _cvmdVirtualMFADeviceName = pVirtualMFADeviceName};

-- | The path for the virtual MFA device. For more information about paths,
-- see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers>
-- in the /Using IAM/ guide.
--
-- This parameter is optional. If it is not included, it defaults to a
-- slash (\/).
cvmdPath :: Lens' CreateVirtualMFADevice (Maybe Text)
cvmdPath = lens _cvmdPath (\ s a -> s{_cvmdPath = a});

-- | The name of the virtual MFA device. Use with path to uniquely identify a
-- virtual MFA device.
cvmdVirtualMFADeviceName :: Lens' CreateVirtualMFADevice Text
cvmdVirtualMFADeviceName = lens _cvmdVirtualMFADeviceName (\ s a -> s{_cvmdVirtualMFADeviceName = a});

instance AWSRequest CreateVirtualMFADevice where
        type Sv CreateVirtualMFADevice = IAM
        type Rs CreateVirtualMFADevice =
             CreateVirtualMFADeviceResponse
        request = post
        response
          = receiveXMLWrapper "CreateVirtualMFADeviceResult"
              (\ s h x ->
                 CreateVirtualMFADeviceResponse' <$>
                   (x .@ "VirtualMFADevice"))

instance ToHeaders CreateVirtualMFADevice where
        toHeaders = const mempty

instance ToPath CreateVirtualMFADevice where
        toPath = const "/"

instance ToQuery CreateVirtualMFADevice where
        toQuery CreateVirtualMFADevice'{..}
          = mconcat
              ["Action" =:
                 ("CreateVirtualMFADevice" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "Path" =: _cvmdPath,
               "VirtualMFADeviceName" =: _cvmdVirtualMFADeviceName]

-- | /See:/ 'createVirtualMFADeviceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvmdrVirtualMFADevice'
newtype CreateVirtualMFADeviceResponse = CreateVirtualMFADeviceResponse'{_cvmdrVirtualMFADevice :: VirtualMFADevice} deriving (Eq, Read, Show)

-- | 'CreateVirtualMFADeviceResponse' smart constructor.
createVirtualMFADeviceResponse :: VirtualMFADevice -> CreateVirtualMFADeviceResponse
createVirtualMFADeviceResponse pVirtualMFADevice = CreateVirtualMFADeviceResponse'{_cvmdrVirtualMFADevice = pVirtualMFADevice};

-- | A newly created virtual MFA device.
cvmdrVirtualMFADevice :: Lens' CreateVirtualMFADeviceResponse VirtualMFADevice
cvmdrVirtualMFADevice = lens _cvmdrVirtualMFADevice (\ s a -> s{_cvmdrVirtualMFADevice = a});
