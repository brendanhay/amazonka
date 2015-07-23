{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeactivateMFADevice
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deactivates the specified MFA device and removes it from association
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
    , dmdrqUserName
    , dmdrqSerialNumber

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
-- * 'dmdrqUserName'
--
-- * 'dmdrqSerialNumber'
data DeactivateMFADevice = DeactivateMFADevice'
    { _dmdrqUserName     :: !Text
    , _dmdrqSerialNumber :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeactivateMFADevice' smart constructor.
deactivateMFADevice :: Text -> Text -> DeactivateMFADevice
deactivateMFADevice pUserName_ pSerialNumber_ =
    DeactivateMFADevice'
    { _dmdrqUserName = pUserName_
    , _dmdrqSerialNumber = pSerialNumber_
    }

-- | The name of the user whose MFA device you want to deactivate.
dmdrqUserName :: Lens' DeactivateMFADevice Text
dmdrqUserName = lens _dmdrqUserName (\ s a -> s{_dmdrqUserName = a});

-- | The serial number that uniquely identifies the MFA device. For virtual
-- MFA devices, the serial number is the device ARN.
dmdrqSerialNumber :: Lens' DeactivateMFADevice Text
dmdrqSerialNumber = lens _dmdrqSerialNumber (\ s a -> s{_dmdrqSerialNumber = a});

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
               "UserName" =: _dmdrqUserName,
               "SerialNumber" =: _dmdrqSerialNumber]

-- | /See:/ 'deactivateMFADeviceResponse' smart constructor.
data DeactivateMFADeviceResponse =
    DeactivateMFADeviceResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeactivateMFADeviceResponse' smart constructor.
deactivateMFADeviceResponse :: DeactivateMFADeviceResponse
deactivateMFADeviceResponse = DeactivateMFADeviceResponse'
