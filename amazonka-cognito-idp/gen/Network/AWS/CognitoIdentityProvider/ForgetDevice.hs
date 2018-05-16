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
-- Module      : Network.AWS.CognitoIdentityProvider.ForgetDevice
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Forgets the specified device.
--
--
module Network.AWS.CognitoIdentityProvider.ForgetDevice
    (
    -- * Creating a Request
      forgetDevice
    , ForgetDevice
    -- * Request Lenses
    , fdAccessToken
    , fdDeviceKey

    -- * Destructuring the Response
    , forgetDeviceResponse
    , ForgetDeviceResponse
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to forget the device.
--
--
--
-- /See:/ 'forgetDevice' smart constructor.
data ForgetDevice = ForgetDevice'
  { _fdAccessToken :: !(Maybe (Sensitive Text))
  , _fdDeviceKey   :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ForgetDevice' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fdAccessToken' - The access token for the forgotten device request.
--
-- * 'fdDeviceKey' - The device key.
forgetDevice
    :: Text -- ^ 'fdDeviceKey'
    -> ForgetDevice
forgetDevice pDeviceKey_ =
  ForgetDevice' {_fdAccessToken = Nothing, _fdDeviceKey = pDeviceKey_}


-- | The access token for the forgotten device request.
fdAccessToken :: Lens' ForgetDevice (Maybe Text)
fdAccessToken = lens _fdAccessToken (\ s a -> s{_fdAccessToken = a}) . mapping _Sensitive

-- | The device key.
fdDeviceKey :: Lens' ForgetDevice Text
fdDeviceKey = lens _fdDeviceKey (\ s a -> s{_fdDeviceKey = a})

instance AWSRequest ForgetDevice where
        type Rs ForgetDevice = ForgetDeviceResponse
        request = postJSON cognitoIdentityProvider
        response = receiveNull ForgetDeviceResponse'

instance Hashable ForgetDevice where

instance NFData ForgetDevice where

instance ToHeaders ForgetDevice where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.ForgetDevice" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ForgetDevice where
        toJSON ForgetDevice'{..}
          = object
              (catMaybes
                 [("AccessToken" .=) <$> _fdAccessToken,
                  Just ("DeviceKey" .= _fdDeviceKey)])

instance ToPath ForgetDevice where
        toPath = const "/"

instance ToQuery ForgetDevice where
        toQuery = const mempty

-- | /See:/ 'forgetDeviceResponse' smart constructor.
data ForgetDeviceResponse =
  ForgetDeviceResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ForgetDeviceResponse' with the minimum fields required to make a request.
--
forgetDeviceResponse
    :: ForgetDeviceResponse
forgetDeviceResponse = ForgetDeviceResponse'


instance NFData ForgetDeviceResponse where
