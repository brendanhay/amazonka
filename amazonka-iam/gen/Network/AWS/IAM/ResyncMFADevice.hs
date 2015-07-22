{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ResyncMFADevice
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Synchronizes the specified MFA device with AWS servers.
--
-- For more information about creating and working with virtual MFA
-- devices, go to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_VirtualMFA.html Using a Virtual MFA Device>
-- in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_ResyncMFADevice.html>
module Network.AWS.IAM.ResyncMFADevice
    (
    -- * Request
      ResyncMFADevice
    -- ** Request constructor
    , resyncMFADevice
    -- ** Request lenses
    , rmdrqUserName
    , rmdrqSerialNumber
    , rmdrqAuthenticationCode1
    , rmdrqAuthenticationCode2

    -- * Response
    , ResyncMFADeviceResponse
    -- ** Response constructor
    , resyncMFADeviceResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'resyncMFADevice' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rmdrqUserName'
--
-- * 'rmdrqSerialNumber'
--
-- * 'rmdrqAuthenticationCode1'
--
-- * 'rmdrqAuthenticationCode2'
data ResyncMFADevice = ResyncMFADevice'
    { _rmdrqUserName            :: !Text
    , _rmdrqSerialNumber        :: !Text
    , _rmdrqAuthenticationCode1 :: !Text
    , _rmdrqAuthenticationCode2 :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ResyncMFADevice' smart constructor.
resyncMFADevice :: Text -> Text -> Text -> Text -> ResyncMFADevice
resyncMFADevice pUserName pSerialNumber pAuthenticationCode1 pAuthenticationCode2 =
    ResyncMFADevice'
    { _rmdrqUserName = pUserName
    , _rmdrqSerialNumber = pSerialNumber
    , _rmdrqAuthenticationCode1 = pAuthenticationCode1
    , _rmdrqAuthenticationCode2 = pAuthenticationCode2
    }

-- | The name of the user whose MFA device you want to resynchronize.
rmdrqUserName :: Lens' ResyncMFADevice Text
rmdrqUserName = lens _rmdrqUserName (\ s a -> s{_rmdrqUserName = a});

-- | Serial number that uniquely identifies the MFA device.
rmdrqSerialNumber :: Lens' ResyncMFADevice Text
rmdrqSerialNumber = lens _rmdrqSerialNumber (\ s a -> s{_rmdrqSerialNumber = a});

-- | An authentication code emitted by the device.
rmdrqAuthenticationCode1 :: Lens' ResyncMFADevice Text
rmdrqAuthenticationCode1 = lens _rmdrqAuthenticationCode1 (\ s a -> s{_rmdrqAuthenticationCode1 = a});

-- | A subsequent authentication code emitted by the device.
rmdrqAuthenticationCode2 :: Lens' ResyncMFADevice Text
rmdrqAuthenticationCode2 = lens _rmdrqAuthenticationCode2 (\ s a -> s{_rmdrqAuthenticationCode2 = a});

instance AWSRequest ResyncMFADevice where
        type Sv ResyncMFADevice = IAM
        type Rs ResyncMFADevice = ResyncMFADeviceResponse
        request = post
        response = receiveNull ResyncMFADeviceResponse'

instance ToHeaders ResyncMFADevice where
        toHeaders = const mempty

instance ToPath ResyncMFADevice where
        toPath = const "/"

instance ToQuery ResyncMFADevice where
        toQuery ResyncMFADevice'{..}
          = mconcat
              ["Action" =: ("ResyncMFADevice" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "UserName" =: _rmdrqUserName,
               "SerialNumber" =: _rmdrqSerialNumber,
               "AuthenticationCode1" =: _rmdrqAuthenticationCode1,
               "AuthenticationCode2" =: _rmdrqAuthenticationCode2]

-- | /See:/ 'resyncMFADeviceResponse' smart constructor.
data ResyncMFADeviceResponse =
    ResyncMFADeviceResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ResyncMFADeviceResponse' smart constructor.
resyncMFADeviceResponse :: ResyncMFADeviceResponse
resyncMFADeviceResponse = ResyncMFADeviceResponse'
