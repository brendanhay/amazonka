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
    , rmdUserName
    , rmdSerialNumber
    , rmdAuthenticationCode1
    , rmdAuthenticationCode2

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
-- * 'rmdUserName'
--
-- * 'rmdSerialNumber'
--
-- * 'rmdAuthenticationCode1'
--
-- * 'rmdAuthenticationCode2'
data ResyncMFADevice = ResyncMFADevice'
    { _rmdUserName            :: !Text
    , _rmdSerialNumber        :: !Text
    , _rmdAuthenticationCode1 :: !Text
    , _rmdAuthenticationCode2 :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ResyncMFADevice' smart constructor.
resyncMFADevice :: Text -> Text -> Text -> Text -> ResyncMFADevice
resyncMFADevice pUserName pSerialNumber pAuthenticationCode1 pAuthenticationCode2 =
    ResyncMFADevice'
    { _rmdUserName = pUserName
    , _rmdSerialNumber = pSerialNumber
    , _rmdAuthenticationCode1 = pAuthenticationCode1
    , _rmdAuthenticationCode2 = pAuthenticationCode2
    }

-- | The name of the user whose MFA device you want to resynchronize.
rmdUserName :: Lens' ResyncMFADevice Text
rmdUserName = lens _rmdUserName (\ s a -> s{_rmdUserName = a});

-- | Serial number that uniquely identifies the MFA device.
rmdSerialNumber :: Lens' ResyncMFADevice Text
rmdSerialNumber = lens _rmdSerialNumber (\ s a -> s{_rmdSerialNumber = a});

-- | An authentication code emitted by the device.
rmdAuthenticationCode1 :: Lens' ResyncMFADevice Text
rmdAuthenticationCode1 = lens _rmdAuthenticationCode1 (\ s a -> s{_rmdAuthenticationCode1 = a});

-- | A subsequent authentication code emitted by the device.
rmdAuthenticationCode2 :: Lens' ResyncMFADevice Text
rmdAuthenticationCode2 = lens _rmdAuthenticationCode2 (\ s a -> s{_rmdAuthenticationCode2 = a});

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
               "UserName" =: _rmdUserName,
               "SerialNumber" =: _rmdSerialNumber,
               "AuthenticationCode1" =: _rmdAuthenticationCode1,
               "AuthenticationCode2" =: _rmdAuthenticationCode2]

-- | /See:/ 'resyncMFADeviceResponse' smart constructor.
data ResyncMFADeviceResponse =
    ResyncMFADeviceResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ResyncMFADeviceResponse' smart constructor.
resyncMFADeviceResponse :: ResyncMFADeviceResponse
resyncMFADeviceResponse = ResyncMFADeviceResponse'
