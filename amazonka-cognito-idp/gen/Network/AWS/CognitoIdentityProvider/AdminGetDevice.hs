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
-- Module      : Network.AWS.CognitoIdentityProvider.AdminGetDevice
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the device, as an administrator.
--
--
-- Requires developer credentials.
--
module Network.AWS.CognitoIdentityProvider.AdminGetDevice
    (
    -- * Creating a Request
      adminGetDevice
    , AdminGetDevice
    -- * Request Lenses
    , agdDeviceKey
    , agdUserPoolId
    , agdUsername

    -- * Destructuring the Response
    , adminGetDeviceResponse
    , AdminGetDeviceResponse
    -- * Response Lenses
    , agdrsResponseStatus
    , agdrsDevice
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to get the device, as an administrator.
--
--
--
-- /See:/ 'adminGetDevice' smart constructor.
data AdminGetDevice = AdminGetDevice'
  { _agdDeviceKey  :: !Text
  , _agdUserPoolId :: !Text
  , _agdUsername   :: !(Sensitive Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdminGetDevice' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'agdDeviceKey' - The device key.
--
-- * 'agdUserPoolId' - The user pool ID.
--
-- * 'agdUsername' - The user name.
adminGetDevice
    :: Text -- ^ 'agdDeviceKey'
    -> Text -- ^ 'agdUserPoolId'
    -> Text -- ^ 'agdUsername'
    -> AdminGetDevice
adminGetDevice pDeviceKey_ pUserPoolId_ pUsername_ =
  AdminGetDevice'
    { _agdDeviceKey = pDeviceKey_
    , _agdUserPoolId = pUserPoolId_
    , _agdUsername = _Sensitive # pUsername_
    }


-- | The device key.
agdDeviceKey :: Lens' AdminGetDevice Text
agdDeviceKey = lens _agdDeviceKey (\ s a -> s{_agdDeviceKey = a})

-- | The user pool ID.
agdUserPoolId :: Lens' AdminGetDevice Text
agdUserPoolId = lens _agdUserPoolId (\ s a -> s{_agdUserPoolId = a})

-- | The user name.
agdUsername :: Lens' AdminGetDevice Text
agdUsername = lens _agdUsername (\ s a -> s{_agdUsername = a}) . _Sensitive

instance AWSRequest AdminGetDevice where
        type Rs AdminGetDevice = AdminGetDeviceResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 AdminGetDeviceResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "Device"))

instance Hashable AdminGetDevice where

instance NFData AdminGetDevice where

instance ToHeaders AdminGetDevice where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.AdminGetDevice"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AdminGetDevice where
        toJSON AdminGetDevice'{..}
          = object
              (catMaybes
                 [Just ("DeviceKey" .= _agdDeviceKey),
                  Just ("UserPoolId" .= _agdUserPoolId),
                  Just ("Username" .= _agdUsername)])

instance ToPath AdminGetDevice where
        toPath = const "/"

instance ToQuery AdminGetDevice where
        toQuery = const mempty

-- | Gets the device response, as an administrator.
--
--
--
-- /See:/ 'adminGetDeviceResponse' smart constructor.
data AdminGetDeviceResponse = AdminGetDeviceResponse'
  { _agdrsResponseStatus :: !Int
  , _agdrsDevice         :: !DeviceType
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdminGetDeviceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'agdrsResponseStatus' - -- | The response status code.
--
-- * 'agdrsDevice' - The device.
adminGetDeviceResponse
    :: Int -- ^ 'agdrsResponseStatus'
    -> DeviceType -- ^ 'agdrsDevice'
    -> AdminGetDeviceResponse
adminGetDeviceResponse pResponseStatus_ pDevice_ =
  AdminGetDeviceResponse'
    {_agdrsResponseStatus = pResponseStatus_, _agdrsDevice = pDevice_}


-- | -- | The response status code.
agdrsResponseStatus :: Lens' AdminGetDeviceResponse Int
agdrsResponseStatus = lens _agdrsResponseStatus (\ s a -> s{_agdrsResponseStatus = a})

-- | The device.
agdrsDevice :: Lens' AdminGetDeviceResponse DeviceType
agdrsDevice = lens _agdrsDevice (\ s a -> s{_agdrsDevice = a})

instance NFData AdminGetDeviceResponse where
