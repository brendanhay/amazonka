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
-- Module      : Network.AWS.CognitoIdentityProvider.AdminForgetDevice
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Forgets the device, as an administrator.
--
--
-- Requires developer credentials.
--
module Network.AWS.CognitoIdentityProvider.AdminForgetDevice
    (
    -- * Creating a Request
      adminForgetDevice
    , AdminForgetDevice
    -- * Request Lenses
    , afdUserPoolId
    , afdUsername
    , afdDeviceKey

    -- * Destructuring the Response
    , adminForgetDeviceResponse
    , AdminForgetDeviceResponse
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Sends the forgot device request, as an administrator.
--
--
--
-- /See:/ 'adminForgetDevice' smart constructor.
data AdminForgetDevice = AdminForgetDevice'
  { _afdUserPoolId :: !Text
  , _afdUsername   :: !(Sensitive Text)
  , _afdDeviceKey  :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdminForgetDevice' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'afdUserPoolId' - The user pool ID.
--
-- * 'afdUsername' - The user name.
--
-- * 'afdDeviceKey' - The device key.
adminForgetDevice
    :: Text -- ^ 'afdUserPoolId'
    -> Text -- ^ 'afdUsername'
    -> Text -- ^ 'afdDeviceKey'
    -> AdminForgetDevice
adminForgetDevice pUserPoolId_ pUsername_ pDeviceKey_ =
  AdminForgetDevice'
    { _afdUserPoolId = pUserPoolId_
    , _afdUsername = _Sensitive # pUsername_
    , _afdDeviceKey = pDeviceKey_
    }


-- | The user pool ID.
afdUserPoolId :: Lens' AdminForgetDevice Text
afdUserPoolId = lens _afdUserPoolId (\ s a -> s{_afdUserPoolId = a})

-- | The user name.
afdUsername :: Lens' AdminForgetDevice Text
afdUsername = lens _afdUsername (\ s a -> s{_afdUsername = a}) . _Sensitive

-- | The device key.
afdDeviceKey :: Lens' AdminForgetDevice Text
afdDeviceKey = lens _afdDeviceKey (\ s a -> s{_afdDeviceKey = a})

instance AWSRequest AdminForgetDevice where
        type Rs AdminForgetDevice = AdminForgetDeviceResponse
        request = postJSON cognitoIdentityProvider
        response = receiveNull AdminForgetDeviceResponse'

instance Hashable AdminForgetDevice where

instance NFData AdminForgetDevice where

instance ToHeaders AdminForgetDevice where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.AdminForgetDevice"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON AdminForgetDevice where
        toJSON AdminForgetDevice'{..}
          = object
              (catMaybes
                 [Just ("UserPoolId" .= _afdUserPoolId),
                  Just ("Username" .= _afdUsername),
                  Just ("DeviceKey" .= _afdDeviceKey)])

instance ToPath AdminForgetDevice where
        toPath = const "/"

instance ToQuery AdminForgetDevice where
        toQuery = const mempty

-- | /See:/ 'adminForgetDeviceResponse' smart constructor.
data AdminForgetDeviceResponse =
  AdminForgetDeviceResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AdminForgetDeviceResponse' with the minimum fields required to make a request.
--
adminForgetDeviceResponse
    :: AdminForgetDeviceResponse
adminForgetDeviceResponse = AdminForgetDeviceResponse'


instance NFData AdminForgetDeviceResponse where
