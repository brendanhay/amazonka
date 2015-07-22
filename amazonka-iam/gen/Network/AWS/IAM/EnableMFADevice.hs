{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.EnableMFADevice
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Enables the specified MFA device and associates it with the specified
-- user name. When enabled, the MFA device is required for every subsequent
-- login by the user name associated with the device.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_EnableMFADevice.html>
module Network.AWS.IAM.EnableMFADevice
    (
    -- * Request
      EnableMFADevice
    -- ** Request constructor
    , enableMFADevice
    -- ** Request lenses
    , emdrqUserName
    , emdrqSerialNumber
    , emdrqAuthenticationCode1
    , emdrqAuthenticationCode2

    -- * Response
    , EnableMFADeviceResponse
    -- ** Response constructor
    , enableMFADeviceResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'enableMFADevice' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'emdrqUserName'
--
-- * 'emdrqSerialNumber'
--
-- * 'emdrqAuthenticationCode1'
--
-- * 'emdrqAuthenticationCode2'
data EnableMFADevice = EnableMFADevice'
    { _emdrqUserName            :: !Text
    , _emdrqSerialNumber        :: !Text
    , _emdrqAuthenticationCode1 :: !Text
    , _emdrqAuthenticationCode2 :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnableMFADevice' smart constructor.
enableMFADevice :: Text -> Text -> Text -> Text -> EnableMFADevice
enableMFADevice pUserName_ pSerialNumber_ pAuthenticationCode1_ pAuthenticationCode2_ =
    EnableMFADevice'
    { _emdrqUserName = pUserName_
    , _emdrqSerialNumber = pSerialNumber_
    , _emdrqAuthenticationCode1 = pAuthenticationCode1_
    , _emdrqAuthenticationCode2 = pAuthenticationCode2_
    }

-- | The name of the user for whom you want to enable the MFA device.
emdrqUserName :: Lens' EnableMFADevice Text
emdrqUserName = lens _emdrqUserName (\ s a -> s{_emdrqUserName = a});

-- | The serial number that uniquely identifies the MFA device. For virtual
-- MFA devices, the serial number is the device ARN.
emdrqSerialNumber :: Lens' EnableMFADevice Text
emdrqSerialNumber = lens _emdrqSerialNumber (\ s a -> s{_emdrqSerialNumber = a});

-- | An authentication code emitted by the device.
emdrqAuthenticationCode1 :: Lens' EnableMFADevice Text
emdrqAuthenticationCode1 = lens _emdrqAuthenticationCode1 (\ s a -> s{_emdrqAuthenticationCode1 = a});

-- | A subsequent authentication code emitted by the device.
emdrqAuthenticationCode2 :: Lens' EnableMFADevice Text
emdrqAuthenticationCode2 = lens _emdrqAuthenticationCode2 (\ s a -> s{_emdrqAuthenticationCode2 = a});

instance AWSRequest EnableMFADevice where
        type Sv EnableMFADevice = IAM
        type Rs EnableMFADevice = EnableMFADeviceResponse
        request = post
        response = receiveNull EnableMFADeviceResponse'

instance ToHeaders EnableMFADevice where
        toHeaders = const mempty

instance ToPath EnableMFADevice where
        toPath = const "/"

instance ToQuery EnableMFADevice where
        toQuery EnableMFADevice'{..}
          = mconcat
              ["Action" =: ("EnableMFADevice" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "UserName" =: _emdrqUserName,
               "SerialNumber" =: _emdrqSerialNumber,
               "AuthenticationCode1" =: _emdrqAuthenticationCode1,
               "AuthenticationCode2" =: _emdrqAuthenticationCode2]

-- | /See:/ 'enableMFADeviceResponse' smart constructor.
data EnableMFADeviceResponse =
    EnableMFADeviceResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnableMFADeviceResponse' smart constructor.
enableMFADeviceResponse :: EnableMFADeviceResponse
enableMFADeviceResponse = EnableMFADeviceResponse'
