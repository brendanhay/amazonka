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
-- Module      : Network.AWS.IAM.DeactivateMFADevice
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
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
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeactivateMFADevice.html AWS API Reference> for DeactivateMFADevice.
module Network.AWS.IAM.DeactivateMFADevice
    (
    -- * Creating a Request
      deactivateMFADevice
    , DeactivateMFADevice
    -- * Request Lenses
    , dmdUserName
    , dmdSerialNumber

    -- * Destructuring the Response
    , deactivateMFADeviceResponse
    , DeactivateMFADeviceResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deactivateMFADevice' smart constructor.
data DeactivateMFADevice = DeactivateMFADevice'
    { _dmdUserName     :: !Text
    , _dmdSerialNumber :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeactivateMFADevice' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmdUserName'
--
-- * 'dmdSerialNumber'
deactivateMFADevice
    :: Text -- ^ 'dmdUserName'
    -> Text -- ^ 'dmdSerialNumber'
    -> DeactivateMFADevice
deactivateMFADevice pUserName_ pSerialNumber_ =
    DeactivateMFADevice'
    { _dmdUserName = pUserName_
    , _dmdSerialNumber = pSerialNumber_
    }

-- | The name of the user whose MFA device you want to deactivate.
dmdUserName :: Lens' DeactivateMFADevice Text
dmdUserName = lens _dmdUserName (\ s a -> s{_dmdUserName = a});

-- | The serial number that uniquely identifies the MFA device. For virtual
-- MFA devices, the serial number is the device ARN.
dmdSerialNumber :: Lens' DeactivateMFADevice Text
dmdSerialNumber = lens _dmdSerialNumber (\ s a -> s{_dmdSerialNumber = a});

instance AWSRequest DeactivateMFADevice where
        type Rs DeactivateMFADevice =
             DeactivateMFADeviceResponse
        request = postQuery iAM
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
               "UserName" =: _dmdUserName,
               "SerialNumber" =: _dmdSerialNumber]

-- | /See:/ 'deactivateMFADeviceResponse' smart constructor.
data DeactivateMFADeviceResponse =
    DeactivateMFADeviceResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeactivateMFADeviceResponse' with the minimum fields required to make a request.
--
deactivateMFADeviceResponse
    :: DeactivateMFADeviceResponse
deactivateMFADeviceResponse = DeactivateMFADeviceResponse'
