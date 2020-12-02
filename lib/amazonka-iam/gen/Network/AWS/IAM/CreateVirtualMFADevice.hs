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
-- Module      : Network.AWS.IAM.CreateVirtualMFADevice
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new virtual MFA device for the AWS account. After creating the virtual MFA, use 'EnableMFADevice' to attach the MFA device to an IAM user. For more information about creating and working with virtual MFA devices, go to <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_VirtualMFA.html Using a Virtual MFA Device> in the /IAM User Guide/ .
--
--
-- For information about limits on the number of MFA devices you can create, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html Limitations on Entities> in the /IAM User Guide/ .
--
-- /Important:/ The seed information contained in the QR code and the Base32 string should be treated like any other secret access information, such as your AWS access keys or your passwords. After you provision your virtual device, you should ensure that the information is destroyed following secure procedures.
--
module Network.AWS.IAM.CreateVirtualMFADevice
    (
    -- * Creating a Request
      createVirtualMFADevice
    , CreateVirtualMFADevice
    -- * Request Lenses
    , cvmdPath
    , cvmdVirtualMFADeviceName

    -- * Destructuring the Response
    , createVirtualMFADeviceResponse
    , CreateVirtualMFADeviceResponse
    -- * Response Lenses
    , cvmdrsResponseStatus
    , cvmdrsVirtualMFADevice
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createVirtualMFADevice' smart constructor.
data CreateVirtualMFADevice = CreateVirtualMFADevice'
  { _cvmdPath                 :: !(Maybe Text)
  , _cvmdVirtualMFADeviceName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateVirtualMFADevice' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvmdPath' - The path for the virtual MFA device. For more information about paths, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ . This parameter is optional. If it is not included, it defaults to a slash (/). This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (\u0021) through the DEL character (\u007F), including most punctuation characters, digits, and upper and lowercased letters.
--
-- * 'cvmdVirtualMFADeviceName' - The name of the virtual MFA device. Use with path to uniquely identify a virtual MFA device. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
createVirtualMFADevice
    :: Text -- ^ 'cvmdVirtualMFADeviceName'
    -> CreateVirtualMFADevice
createVirtualMFADevice pVirtualMFADeviceName_ =
  CreateVirtualMFADevice'
    {_cvmdPath = Nothing, _cvmdVirtualMFADeviceName = pVirtualMFADeviceName_}


-- | The path for the virtual MFA device. For more information about paths, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM Identifiers> in the /IAM User Guide/ . This parameter is optional. If it is not included, it defaults to a slash (/). This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (\u0021) through the DEL character (\u007F), including most punctuation characters, digits, and upper and lowercased letters.
cvmdPath :: Lens' CreateVirtualMFADevice (Maybe Text)
cvmdPath = lens _cvmdPath (\ s a -> s{_cvmdPath = a})

-- | The name of the virtual MFA device. Use with path to uniquely identify a virtual MFA device. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
cvmdVirtualMFADeviceName :: Lens' CreateVirtualMFADevice Text
cvmdVirtualMFADeviceName = lens _cvmdVirtualMFADeviceName (\ s a -> s{_cvmdVirtualMFADeviceName = a})

instance AWSRequest CreateVirtualMFADevice where
        type Rs CreateVirtualMFADevice =
             CreateVirtualMFADeviceResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "CreateVirtualMFADeviceResult"
              (\ s h x ->
                 CreateVirtualMFADeviceResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "VirtualMFADevice"))

instance Hashable CreateVirtualMFADevice where

instance NFData CreateVirtualMFADevice where

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

-- | Contains the response to a successful 'CreateVirtualMFADevice' request.
--
--
--
-- /See:/ 'createVirtualMFADeviceResponse' smart constructor.
data CreateVirtualMFADeviceResponse = CreateVirtualMFADeviceResponse'
  { _cvmdrsResponseStatus   :: !Int
  , _cvmdrsVirtualMFADevice :: !VirtualMFADevice
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateVirtualMFADeviceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cvmdrsResponseStatus' - -- | The response status code.
--
-- * 'cvmdrsVirtualMFADevice' - A structure containing details about the new virtual MFA device.
createVirtualMFADeviceResponse
    :: Int -- ^ 'cvmdrsResponseStatus'
    -> VirtualMFADevice -- ^ 'cvmdrsVirtualMFADevice'
    -> CreateVirtualMFADeviceResponse
createVirtualMFADeviceResponse pResponseStatus_ pVirtualMFADevice_ =
  CreateVirtualMFADeviceResponse'
    { _cvmdrsResponseStatus = pResponseStatus_
    , _cvmdrsVirtualMFADevice = pVirtualMFADevice_
    }


-- | -- | The response status code.
cvmdrsResponseStatus :: Lens' CreateVirtualMFADeviceResponse Int
cvmdrsResponseStatus = lens _cvmdrsResponseStatus (\ s a -> s{_cvmdrsResponseStatus = a})

-- | A structure containing details about the new virtual MFA device.
cvmdrsVirtualMFADevice :: Lens' CreateVirtualMFADeviceResponse VirtualMFADevice
cvmdrsVirtualMFADevice = lens _cvmdrsVirtualMFADevice (\ s a -> s{_cvmdrsVirtualMFADevice = a})

instance NFData CreateVirtualMFADeviceResponse where
