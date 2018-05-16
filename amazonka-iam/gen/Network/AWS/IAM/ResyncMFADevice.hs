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
-- Module      : Network.AWS.IAM.ResyncMFADevice
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Synchronizes the specified MFA device with its IAM resource object on the AWS servers.
--
--
-- For more information about creating and working with virtual MFA devices, go to <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_VirtualMFA.html Using a Virtual MFA Device> in the /IAM User Guide/ .
--
module Network.AWS.IAM.ResyncMFADevice
    (
    -- * Creating a Request
      resyncMFADevice
    , ResyncMFADevice
    -- * Request Lenses
    , rmdUserName
    , rmdSerialNumber
    , rmdAuthenticationCode1
    , rmdAuthenticationCode2

    -- * Destructuring the Response
    , resyncMFADeviceResponse
    , ResyncMFADeviceResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'resyncMFADevice' smart constructor.
data ResyncMFADevice = ResyncMFADevice'
  { _rmdUserName            :: !Text
  , _rmdSerialNumber        :: !Text
  , _rmdAuthenticationCode1 :: !Text
  , _rmdAuthenticationCode2 :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResyncMFADevice' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rmdUserName' - The name of the user whose MFA device you want to resynchronize. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- * 'rmdSerialNumber' - Serial number that uniquely identifies the MFA device. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- * 'rmdAuthenticationCode1' - An authentication code emitted by the device. The format for this parameter is a sequence of six digits.
--
-- * 'rmdAuthenticationCode2' - A subsequent authentication code emitted by the device. The format for this parameter is a sequence of six digits.
resyncMFADevice
    :: Text -- ^ 'rmdUserName'
    -> Text -- ^ 'rmdSerialNumber'
    -> Text -- ^ 'rmdAuthenticationCode1'
    -> Text -- ^ 'rmdAuthenticationCode2'
    -> ResyncMFADevice
resyncMFADevice pUserName_ pSerialNumber_ pAuthenticationCode1_ pAuthenticationCode2_ =
  ResyncMFADevice'
    { _rmdUserName = pUserName_
    , _rmdSerialNumber = pSerialNumber_
    , _rmdAuthenticationCode1 = pAuthenticationCode1_
    , _rmdAuthenticationCode2 = pAuthenticationCode2_
    }


-- | The name of the user whose MFA device you want to resynchronize. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
rmdUserName :: Lens' ResyncMFADevice Text
rmdUserName = lens _rmdUserName (\ s a -> s{_rmdUserName = a})

-- | Serial number that uniquely identifies the MFA device. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
rmdSerialNumber :: Lens' ResyncMFADevice Text
rmdSerialNumber = lens _rmdSerialNumber (\ s a -> s{_rmdSerialNumber = a})

-- | An authentication code emitted by the device. The format for this parameter is a sequence of six digits.
rmdAuthenticationCode1 :: Lens' ResyncMFADevice Text
rmdAuthenticationCode1 = lens _rmdAuthenticationCode1 (\ s a -> s{_rmdAuthenticationCode1 = a})

-- | A subsequent authentication code emitted by the device. The format for this parameter is a sequence of six digits.
rmdAuthenticationCode2 :: Lens' ResyncMFADevice Text
rmdAuthenticationCode2 = lens _rmdAuthenticationCode2 (\ s a -> s{_rmdAuthenticationCode2 = a})

instance AWSRequest ResyncMFADevice where
        type Rs ResyncMFADevice = ResyncMFADeviceResponse
        request = postQuery iam
        response = receiveNull ResyncMFADeviceResponse'

instance Hashable ResyncMFADevice where

instance NFData ResyncMFADevice where

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
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ResyncMFADeviceResponse' with the minimum fields required to make a request.
--
resyncMFADeviceResponse
    :: ResyncMFADeviceResponse
resyncMFADeviceResponse = ResyncMFADeviceResponse'


instance NFData ResyncMFADeviceResponse where
