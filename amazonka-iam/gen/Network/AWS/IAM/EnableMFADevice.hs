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
    , emdUserName
    , emdSerialNumber
    , emdAuthenticationCode1
    , emdAuthenticationCode2

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
-- * 'emdUserName'
--
-- * 'emdSerialNumber'
--
-- * 'emdAuthenticationCode1'
--
-- * 'emdAuthenticationCode2'
data EnableMFADevice = EnableMFADevice'
    { _emdUserName            :: !Text
    , _emdSerialNumber        :: !Text
    , _emdAuthenticationCode1 :: !Text
    , _emdAuthenticationCode2 :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnableMFADevice' smart constructor.
enableMFADevice :: Text -> Text -> Text -> Text -> EnableMFADevice
enableMFADevice pUserName pSerialNumber pAuthenticationCode1 pAuthenticationCode2 =
    EnableMFADevice'
    { _emdUserName = pUserName
    , _emdSerialNumber = pSerialNumber
    , _emdAuthenticationCode1 = pAuthenticationCode1
    , _emdAuthenticationCode2 = pAuthenticationCode2
    }

-- | The name of the user for whom you want to enable the MFA device.
emdUserName :: Lens' EnableMFADevice Text
emdUserName = lens _emdUserName (\ s a -> s{_emdUserName = a});

-- | The serial number that uniquely identifies the MFA device. For virtual
-- MFA devices, the serial number is the device ARN.
emdSerialNumber :: Lens' EnableMFADevice Text
emdSerialNumber = lens _emdSerialNumber (\ s a -> s{_emdSerialNumber = a});

-- | An authentication code emitted by the device.
emdAuthenticationCode1 :: Lens' EnableMFADevice Text
emdAuthenticationCode1 = lens _emdAuthenticationCode1 (\ s a -> s{_emdAuthenticationCode1 = a});

-- | A subsequent authentication code emitted by the device.
emdAuthenticationCode2 :: Lens' EnableMFADevice Text
emdAuthenticationCode2 = lens _emdAuthenticationCode2 (\ s a -> s{_emdAuthenticationCode2 = a});

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
               "UserName" =: _emdUserName,
               "SerialNumber" =: _emdSerialNumber,
               "AuthenticationCode1" =: _emdAuthenticationCode1,
               "AuthenticationCode2" =: _emdAuthenticationCode2]

-- | /See:/ 'enableMFADeviceResponse' smart constructor.
data EnableMFADeviceResponse =
    EnableMFADeviceResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'EnableMFADeviceResponse' smart constructor.
enableMFADeviceResponse :: EnableMFADeviceResponse
enableMFADeviceResponse = EnableMFADeviceResponse'
