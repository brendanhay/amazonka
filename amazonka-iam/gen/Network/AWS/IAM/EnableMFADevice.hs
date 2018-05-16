{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.EnableMFADevice
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the specified MFA device and associates it with the specified IAM user. When enabled, the MFA device is required for every subsequent login by the IAM user associated with the device.
--
--
module Network.AWS.IAM.EnableMFADevice
    (
    -- * Creating a Request
      enableMFADevice
    , EnableMFADevice
    -- * Request Lenses
    , emdUserName
    , emdSerialNumber
    , emdAuthenticationCode1
    , emdAuthenticationCode2

    -- * Destructuring the Response
    , enableMFADeviceResponse
    , EnableMFADeviceResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'enableMFADevice' smart constructor.
data EnableMFADevice = EnableMFADevice'
  { _emdUserName            :: !Text
  , _emdSerialNumber        :: !Text
  , _emdAuthenticationCode1 :: !Text
  , _emdAuthenticationCode2 :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnableMFADevice' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'emdUserName' - The name of the IAM user for whom you want to enable the MFA device. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- * 'emdSerialNumber' - The serial number that uniquely identifies the MFA device. For virtual MFA devices, the serial number is the device ARN. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: =,.@:/-
--
-- * 'emdAuthenticationCode1' - An authentication code emitted by the device.  The format for this parameter is a string of six digits. /Important:/ Submit your request immediately after generating the authentication codes. If you generate the codes and then wait too long to submit the request, the MFA device successfully associates with the user but the MFA device becomes out of sync. This happens because time-based one-time passwords (TOTP) expire after a short period of time. If this happens, you can <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_mfa_sync.html resync the device> .
--
-- * 'emdAuthenticationCode2' - A subsequent authentication code emitted by the device. The format for this parameter is a string of six digits. /Important:/ Submit your request immediately after generating the authentication codes. If you generate the codes and then wait too long to submit the request, the MFA device successfully associates with the user but the MFA device becomes out of sync. This happens because time-based one-time passwords (TOTP) expire after a short period of time. If this happens, you can <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_mfa_sync.html resync the device> .
enableMFADevice
    :: Text -- ^ 'emdUserName'
    -> Text -- ^ 'emdSerialNumber'
    -> Text -- ^ 'emdAuthenticationCode1'
    -> Text -- ^ 'emdAuthenticationCode2'
    -> EnableMFADevice
enableMFADevice pUserName_ pSerialNumber_ pAuthenticationCode1_ pAuthenticationCode2_ =
  EnableMFADevice'
    { _emdUserName = pUserName_
    , _emdSerialNumber = pSerialNumber_
    , _emdAuthenticationCode1 = pAuthenticationCode1_
    , _emdAuthenticationCode2 = pAuthenticationCode2_
    }


-- | The name of the IAM user for whom you want to enable the MFA device. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
emdUserName :: Lens' EnableMFADevice Text
emdUserName = lens _emdUserName (\ s a -> s{_emdUserName = a})

-- | The serial number that uniquely identifies the MFA device. For virtual MFA devices, the serial number is the device ARN. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: =,.@:/-
emdSerialNumber :: Lens' EnableMFADevice Text
emdSerialNumber = lens _emdSerialNumber (\ s a -> s{_emdSerialNumber = a})

-- | An authentication code emitted by the device.  The format for this parameter is a string of six digits. /Important:/ Submit your request immediately after generating the authentication codes. If you generate the codes and then wait too long to submit the request, the MFA device successfully associates with the user but the MFA device becomes out of sync. This happens because time-based one-time passwords (TOTP) expire after a short period of time. If this happens, you can <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_mfa_sync.html resync the device> .
emdAuthenticationCode1 :: Lens' EnableMFADevice Text
emdAuthenticationCode1 = lens _emdAuthenticationCode1 (\ s a -> s{_emdAuthenticationCode1 = a})

-- | A subsequent authentication code emitted by the device. The format for this parameter is a string of six digits. /Important:/ Submit your request immediately after generating the authentication codes. If you generate the codes and then wait too long to submit the request, the MFA device successfully associates with the user but the MFA device becomes out of sync. This happens because time-based one-time passwords (TOTP) expire after a short period of time. If this happens, you can <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_credentials_mfa_sync.html resync the device> .
emdAuthenticationCode2 :: Lens' EnableMFADevice Text
emdAuthenticationCode2 = lens _emdAuthenticationCode2 (\ s a -> s{_emdAuthenticationCode2 = a})

instance AWSRequest EnableMFADevice where
        type Rs EnableMFADevice = EnableMFADeviceResponse
        request = postQuery iam
        response = receiveNull EnableMFADeviceResponse'

instance Hashable EnableMFADevice where

instance NFData EnableMFADevice where

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
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'EnableMFADeviceResponse' with the minimum fields required to make a request.
--
enableMFADeviceResponse
    :: EnableMFADeviceResponse
enableMFADeviceResponse = EnableMFADeviceResponse'


instance NFData EnableMFADeviceResponse where
