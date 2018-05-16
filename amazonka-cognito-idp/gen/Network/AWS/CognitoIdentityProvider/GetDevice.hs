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
-- Module      : Network.AWS.CognitoIdentityProvider.GetDevice
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the device.
--
--
module Network.AWS.CognitoIdentityProvider.GetDevice
    (
    -- * Creating a Request
      getDevice
    , GetDevice
    -- * Request Lenses
    , gdAccessToken
    , gdDeviceKey

    -- * Destructuring the Response
    , getDeviceResponse
    , GetDeviceResponse
    -- * Response Lenses
    , gdrsResponseStatus
    , gdrsDevice
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to get the device.
--
--
--
-- /See:/ 'getDevice' smart constructor.
data GetDevice = GetDevice'
  { _gdAccessToken :: !(Maybe (Sensitive Text))
  , _gdDeviceKey   :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDevice' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdAccessToken' - The access token.
--
-- * 'gdDeviceKey' - The device key.
getDevice
    :: Text -- ^ 'gdDeviceKey'
    -> GetDevice
getDevice pDeviceKey_ =
  GetDevice' {_gdAccessToken = Nothing, _gdDeviceKey = pDeviceKey_}


-- | The access token.
gdAccessToken :: Lens' GetDevice (Maybe Text)
gdAccessToken = lens _gdAccessToken (\ s a -> s{_gdAccessToken = a}) . mapping _Sensitive

-- | The device key.
gdDeviceKey :: Lens' GetDevice Text
gdDeviceKey = lens _gdDeviceKey (\ s a -> s{_gdDeviceKey = a})

instance AWSRequest GetDevice where
        type Rs GetDevice = GetDeviceResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 GetDeviceResponse' <$>
                   (pure (fromEnum s)) <*> (x .:> "Device"))

instance Hashable GetDevice where

instance NFData GetDevice where

instance ToHeaders GetDevice where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.GetDevice" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON GetDevice where
        toJSON GetDevice'{..}
          = object
              (catMaybes
                 [("AccessToken" .=) <$> _gdAccessToken,
                  Just ("DeviceKey" .= _gdDeviceKey)])

instance ToPath GetDevice where
        toPath = const "/"

instance ToQuery GetDevice where
        toQuery = const mempty

-- | Gets the device response.
--
--
--
-- /See:/ 'getDeviceResponse' smart constructor.
data GetDeviceResponse = GetDeviceResponse'
  { _gdrsResponseStatus :: !Int
  , _gdrsDevice         :: !DeviceType
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetDeviceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gdrsResponseStatus' - -- | The response status code.
--
-- * 'gdrsDevice' - The device.
getDeviceResponse
    :: Int -- ^ 'gdrsResponseStatus'
    -> DeviceType -- ^ 'gdrsDevice'
    -> GetDeviceResponse
getDeviceResponse pResponseStatus_ pDevice_ =
  GetDeviceResponse'
    {_gdrsResponseStatus = pResponseStatus_, _gdrsDevice = pDevice_}


-- | -- | The response status code.
gdrsResponseStatus :: Lens' GetDeviceResponse Int
gdrsResponseStatus = lens _gdrsResponseStatus (\ s a -> s{_gdrsResponseStatus = a})

-- | The device.
gdrsDevice :: Lens' GetDeviceResponse DeviceType
gdrsDevice = lens _gdrsDevice (\ s a -> s{_gdrsDevice = a})

instance NFData GetDeviceResponse where
