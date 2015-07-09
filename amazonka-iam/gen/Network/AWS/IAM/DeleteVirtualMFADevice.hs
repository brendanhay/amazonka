{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteVirtualMFADevice
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Deletes a virtual MFA device.
--
-- You must deactivate a user\'s virtual MFA device before you can delete
-- it. For information about deactivating MFA devices, see
-- DeactivateMFADevice.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteVirtualMFADevice.html>
module Network.AWS.IAM.DeleteVirtualMFADevice
    (
    -- * Request
      DeleteVirtualMFADevice
    -- ** Request constructor
    , deleteVirtualMFADevice
    -- ** Request lenses
    , dvmdSerialNumber

    -- * Response
    , DeleteVirtualMFADeviceResponse
    -- ** Response constructor
    , deleteVirtualMFADeviceResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteVirtualMFADevice' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvmdSerialNumber'
newtype DeleteVirtualMFADevice = DeleteVirtualMFADevice'
    { _dvmdSerialNumber :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteVirtualMFADevice' smart constructor.
deleteVirtualMFADevice :: Text -> DeleteVirtualMFADevice
deleteVirtualMFADevice pSerialNumber =
    DeleteVirtualMFADevice'
    { _dvmdSerialNumber = pSerialNumber
    }

-- | The serial number that uniquely identifies the MFA device. For virtual
-- MFA devices, the serial number is the same as the ARN.
dvmdSerialNumber :: Lens' DeleteVirtualMFADevice Text
dvmdSerialNumber = lens _dvmdSerialNumber (\ s a -> s{_dvmdSerialNumber = a});

instance AWSRequest DeleteVirtualMFADevice where
        type Sv DeleteVirtualMFADevice = IAM
        type Rs DeleteVirtualMFADevice =
             DeleteVirtualMFADeviceResponse
        request = post
        response
          = receiveNull DeleteVirtualMFADeviceResponse'

instance ToHeaders DeleteVirtualMFADevice where
        toHeaders = const mempty

instance ToPath DeleteVirtualMFADevice where
        toPath = const "/"

instance ToQuery DeleteVirtualMFADevice where
        toQuery DeleteVirtualMFADevice'{..}
          = mconcat
              ["Action" =:
                 ("DeleteVirtualMFADevice" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "SerialNumber" =: _dvmdSerialNumber]

-- | /See:/ 'deleteVirtualMFADeviceResponse' smart constructor.
data DeleteVirtualMFADeviceResponse =
    DeleteVirtualMFADeviceResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteVirtualMFADeviceResponse' smart constructor.
deleteVirtualMFADeviceResponse :: DeleteVirtualMFADeviceResponse
deleteVirtualMFADeviceResponse = DeleteVirtualMFADeviceResponse'
