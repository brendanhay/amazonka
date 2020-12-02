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
-- Module      : Network.AWS.CognitoIdentityProvider.AdminUpdateDeviceStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the device status as an administrator.
--
--
-- Requires developer credentials.
--
module Network.AWS.CognitoIdentityProvider.AdminUpdateDeviceStatus
    (
    -- * Creating a Request
      adminUpdateDeviceStatus
    , AdminUpdateDeviceStatus
    -- * Request Lenses
    , audsDeviceRememberedStatus
    , audsUserPoolId
    , audsUsername
    , audsDeviceKey

    -- * Destructuring the Response
    , adminUpdateDeviceStatusResponse
    , AdminUpdateDeviceStatusResponse
    -- * Response Lenses
    , audsrsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The request to update the device status, as an administrator.
--
--
--
-- /See:/ 'adminUpdateDeviceStatus' smart constructor.
data AdminUpdateDeviceStatus = AdminUpdateDeviceStatus'
  { _audsDeviceRememberedStatus :: !(Maybe DeviceRememberedStatusType)
  , _audsUserPoolId             :: !Text
  , _audsUsername               :: !(Sensitive Text)
  , _audsDeviceKey              :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdminUpdateDeviceStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'audsDeviceRememberedStatus' - The status indicating whether a device has been remembered or not.
--
-- * 'audsUserPoolId' - The user pool ID.
--
-- * 'audsUsername' - The user name.
--
-- * 'audsDeviceKey' - The device key.
adminUpdateDeviceStatus
    :: Text -- ^ 'audsUserPoolId'
    -> Text -- ^ 'audsUsername'
    -> Text -- ^ 'audsDeviceKey'
    -> AdminUpdateDeviceStatus
adminUpdateDeviceStatus pUserPoolId_ pUsername_ pDeviceKey_ =
  AdminUpdateDeviceStatus'
    { _audsDeviceRememberedStatus = Nothing
    , _audsUserPoolId = pUserPoolId_
    , _audsUsername = _Sensitive # pUsername_
    , _audsDeviceKey = pDeviceKey_
    }


-- | The status indicating whether a device has been remembered or not.
audsDeviceRememberedStatus :: Lens' AdminUpdateDeviceStatus (Maybe DeviceRememberedStatusType)
audsDeviceRememberedStatus = lens _audsDeviceRememberedStatus (\ s a -> s{_audsDeviceRememberedStatus = a})

-- | The user pool ID.
audsUserPoolId :: Lens' AdminUpdateDeviceStatus Text
audsUserPoolId = lens _audsUserPoolId (\ s a -> s{_audsUserPoolId = a})

-- | The user name.
audsUsername :: Lens' AdminUpdateDeviceStatus Text
audsUsername = lens _audsUsername (\ s a -> s{_audsUsername = a}) . _Sensitive

-- | The device key.
audsDeviceKey :: Lens' AdminUpdateDeviceStatus Text
audsDeviceKey = lens _audsDeviceKey (\ s a -> s{_audsDeviceKey = a})

instance AWSRequest AdminUpdateDeviceStatus where
        type Rs AdminUpdateDeviceStatus =
             AdminUpdateDeviceStatusResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveEmpty
              (\ s h x ->
                 AdminUpdateDeviceStatusResponse' <$>
                   (pure (fromEnum s)))

instance Hashable AdminUpdateDeviceStatus where

instance NFData AdminUpdateDeviceStatus where

instance ToHeaders AdminUpdateDeviceStatus where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.AdminUpdateDeviceStatus"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AdminUpdateDeviceStatus where
        toJSON AdminUpdateDeviceStatus'{..}
          = object
              (catMaybes
                 [("DeviceRememberedStatus" .=) <$>
                    _audsDeviceRememberedStatus,
                  Just ("UserPoolId" .= _audsUserPoolId),
                  Just ("Username" .= _audsUsername),
                  Just ("DeviceKey" .= _audsDeviceKey)])

instance ToPath AdminUpdateDeviceStatus where
        toPath = const "/"

instance ToQuery AdminUpdateDeviceStatus where
        toQuery = const mempty

-- | The status response from the request to update the device, as an administrator.
--
--
--
-- /See:/ 'adminUpdateDeviceStatusResponse' smart constructor.
newtype AdminUpdateDeviceStatusResponse = AdminUpdateDeviceStatusResponse'
  { _audsrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdminUpdateDeviceStatusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'audsrsResponseStatus' - -- | The response status code.
adminUpdateDeviceStatusResponse
    :: Int -- ^ 'audsrsResponseStatus'
    -> AdminUpdateDeviceStatusResponse
adminUpdateDeviceStatusResponse pResponseStatus_ =
  AdminUpdateDeviceStatusResponse' {_audsrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
audsrsResponseStatus :: Lens' AdminUpdateDeviceStatusResponse Int
audsrsResponseStatus = lens _audsrsResponseStatus (\ s a -> s{_audsrsResponseStatus = a})

instance NFData AdminUpdateDeviceStatusResponse where
