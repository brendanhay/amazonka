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
-- Module      : Network.AWS.CognitoIdentityProvider.UpdateDeviceStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the device status.
--
--
module Network.AWS.CognitoIdentityProvider.UpdateDeviceStatus
    (
    -- * Creating a Request
      updateDeviceStatus
    , UpdateDeviceStatus
    -- * Request Lenses
    , udsDeviceRememberedStatus
    , udsAccessToken
    , udsDeviceKey

    -- * Destructuring the Response
    , updateDeviceStatusResponse
    , UpdateDeviceStatusResponse
    -- * Response Lenses
    , udsrsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the request to update the device status.
--
--
--
-- /See:/ 'updateDeviceStatus' smart constructor.
data UpdateDeviceStatus = UpdateDeviceStatus'
  { _udsDeviceRememberedStatus :: !(Maybe DeviceRememberedStatusType)
  , _udsAccessToken            :: !(Sensitive Text)
  , _udsDeviceKey              :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDeviceStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udsDeviceRememberedStatus' - The status of whether a device is remembered.
--
-- * 'udsAccessToken' - The access token.
--
-- * 'udsDeviceKey' - The device key.
updateDeviceStatus
    :: Text -- ^ 'udsAccessToken'
    -> Text -- ^ 'udsDeviceKey'
    -> UpdateDeviceStatus
updateDeviceStatus pAccessToken_ pDeviceKey_ =
  UpdateDeviceStatus'
    { _udsDeviceRememberedStatus = Nothing
    , _udsAccessToken = _Sensitive # pAccessToken_
    , _udsDeviceKey = pDeviceKey_
    }


-- | The status of whether a device is remembered.
udsDeviceRememberedStatus :: Lens' UpdateDeviceStatus (Maybe DeviceRememberedStatusType)
udsDeviceRememberedStatus = lens _udsDeviceRememberedStatus (\ s a -> s{_udsDeviceRememberedStatus = a})

-- | The access token.
udsAccessToken :: Lens' UpdateDeviceStatus Text
udsAccessToken = lens _udsAccessToken (\ s a -> s{_udsAccessToken = a}) . _Sensitive

-- | The device key.
udsDeviceKey :: Lens' UpdateDeviceStatus Text
udsDeviceKey = lens _udsDeviceKey (\ s a -> s{_udsDeviceKey = a})

instance AWSRequest UpdateDeviceStatus where
        type Rs UpdateDeviceStatus =
             UpdateDeviceStatusResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateDeviceStatusResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateDeviceStatus where

instance NFData UpdateDeviceStatus where

instance ToHeaders UpdateDeviceStatus where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.UpdateDeviceStatus"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateDeviceStatus where
        toJSON UpdateDeviceStatus'{..}
          = object
              (catMaybes
                 [("DeviceRememberedStatus" .=) <$>
                    _udsDeviceRememberedStatus,
                  Just ("AccessToken" .= _udsAccessToken),
                  Just ("DeviceKey" .= _udsDeviceKey)])

instance ToPath UpdateDeviceStatus where
        toPath = const "/"

instance ToQuery UpdateDeviceStatus where
        toQuery = const mempty

-- | The response to the request to update the device status.
--
--
--
-- /See:/ 'updateDeviceStatusResponse' smart constructor.
newtype UpdateDeviceStatusResponse = UpdateDeviceStatusResponse'
  { _udsrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDeviceStatusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udsrsResponseStatus' - -- | The response status code.
updateDeviceStatusResponse
    :: Int -- ^ 'udsrsResponseStatus'
    -> UpdateDeviceStatusResponse
updateDeviceStatusResponse pResponseStatus_ =
  UpdateDeviceStatusResponse' {_udsrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
udsrsResponseStatus :: Lens' UpdateDeviceStatusResponse Int
udsrsResponseStatus = lens _udsrsResponseStatus (\ s a -> s{_udsrsResponseStatus = a})

instance NFData UpdateDeviceStatusResponse where
