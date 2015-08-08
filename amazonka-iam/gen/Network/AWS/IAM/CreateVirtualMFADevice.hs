{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.CreateVirtualMFADevice
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new virtual MFA device for the AWS account. After creating the
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
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateVirtualMFADevice.html AWS API Reference> for CreateVirtualMFADevice.
module Network.AWS.IAM.CreateVirtualMFADevice
    (
    -- * Creating a Request
      CreateVirtualMFADevice
    , createVirtualMFADevice
    -- * Request Lenses
    , cvmdPath
    , cvmdVirtualMFADeviceName

    -- * Destructuring the Response
    , CreateVirtualMFADeviceResponse
    , createVirtualMFADeviceResponse
    -- * Response Lenses
    , cvmdrsStatus
    , cvmdrsVirtualMFADevice
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createVirtualMFADevice' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvmdPath'
--
-- * 'cvmdVirtualMFADeviceName'
data CreateVirtualMFADevice = CreateVirtualMFADevice'
    { _cvmdPath                 :: !(Maybe Text)
    , _cvmdVirtualMFADeviceName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateVirtualMFADevice' smart constructor.
createVirtualMFADevice :: Text -> CreateVirtualMFADevice
createVirtualMFADevice pVirtualMFADeviceName_ =
    CreateVirtualMFADevice'
    { _cvmdPath = Nothing
    , _cvmdVirtualMFADeviceName = pVirtualMFADeviceName_
    }

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
        request = postQuery
        response
          = receiveXMLWrapper "CreateVirtualMFADeviceResult"
              (\ s h x ->
                 CreateVirtualMFADeviceResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "VirtualMFADevice"))

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

-- | Contains the response to a successful CreateVirtualMFADevice request.
--
-- /See:/ 'createVirtualMFADeviceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cvmdrsStatus'
--
-- * 'cvmdrsVirtualMFADevice'
data CreateVirtualMFADeviceResponse = CreateVirtualMFADeviceResponse'
    { _cvmdrsStatus           :: !Int
    , _cvmdrsVirtualMFADevice :: !VirtualMFADevice
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateVirtualMFADeviceResponse' smart constructor.
createVirtualMFADeviceResponse :: Int -> VirtualMFADevice -> CreateVirtualMFADeviceResponse
createVirtualMFADeviceResponse pStatus_ pVirtualMFADevice_ =
    CreateVirtualMFADeviceResponse'
    { _cvmdrsStatus = pStatus_
    , _cvmdrsVirtualMFADevice = pVirtualMFADevice_
    }

-- | Undocumented member.
cvmdrsStatus :: Lens' CreateVirtualMFADeviceResponse Int
cvmdrsStatus = lens _cvmdrsStatus (\ s a -> s{_cvmdrsStatus = a});

-- | A newly created virtual MFA device.
cvmdrsVirtualMFADevice :: Lens' CreateVirtualMFADeviceResponse VirtualMFADevice
cvmdrsVirtualMFADevice = lens _cvmdrsVirtualMFADevice (\ s a -> s{_cvmdrsVirtualMFADevice = a});
