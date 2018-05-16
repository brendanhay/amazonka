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
-- Module      : Network.AWS.CognitoIdentityProvider.ConfirmDevice
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Confirms tracking of the device. This API call is the call that begins device tracking.
--
--
module Network.AWS.CognitoIdentityProvider.ConfirmDevice
    (
    -- * Creating a Request
      confirmDevice
    , ConfirmDevice
    -- * Request Lenses
    , cdDeviceSecretVerifierConfig
    , cdDeviceName
    , cdAccessToken
    , cdDeviceKey

    -- * Destructuring the Response
    , confirmDeviceResponse
    , ConfirmDeviceResponse
    -- * Response Lenses
    , cdrsUserConfirmationNecessary
    , cdrsResponseStatus
    ) where

import Network.AWS.CognitoIdentityProvider.Types
import Network.AWS.CognitoIdentityProvider.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Confirms the device request.
--
--
--
-- /See:/ 'confirmDevice' smart constructor.
data ConfirmDevice = ConfirmDevice'
  { _cdDeviceSecretVerifierConfig :: !(Maybe DeviceSecretVerifierConfigType)
  , _cdDeviceName                 :: !(Maybe Text)
  , _cdAccessToken                :: !(Sensitive Text)
  , _cdDeviceKey                  :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'ConfirmDevice' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdDeviceSecretVerifierConfig' - The configuration of the device secret verifier.
--
-- * 'cdDeviceName' - The device name.
--
-- * 'cdAccessToken' - The access token.
--
-- * 'cdDeviceKey' - The device key.
confirmDevice
    :: Text -- ^ 'cdAccessToken'
    -> Text -- ^ 'cdDeviceKey'
    -> ConfirmDevice
confirmDevice pAccessToken_ pDeviceKey_ =
  ConfirmDevice'
    { _cdDeviceSecretVerifierConfig = Nothing
    , _cdDeviceName = Nothing
    , _cdAccessToken = _Sensitive # pAccessToken_
    , _cdDeviceKey = pDeviceKey_
    }


-- | The configuration of the device secret verifier.
cdDeviceSecretVerifierConfig :: Lens' ConfirmDevice (Maybe DeviceSecretVerifierConfigType)
cdDeviceSecretVerifierConfig = lens _cdDeviceSecretVerifierConfig (\ s a -> s{_cdDeviceSecretVerifierConfig = a})

-- | The device name.
cdDeviceName :: Lens' ConfirmDevice (Maybe Text)
cdDeviceName = lens _cdDeviceName (\ s a -> s{_cdDeviceName = a})

-- | The access token.
cdAccessToken :: Lens' ConfirmDevice Text
cdAccessToken = lens _cdAccessToken (\ s a -> s{_cdAccessToken = a}) . _Sensitive

-- | The device key.
cdDeviceKey :: Lens' ConfirmDevice Text
cdDeviceKey = lens _cdDeviceKey (\ s a -> s{_cdDeviceKey = a})

instance AWSRequest ConfirmDevice where
        type Rs ConfirmDevice = ConfirmDeviceResponse
        request = postJSON cognitoIdentityProvider
        response
          = receiveJSON
              (\ s h x ->
                 ConfirmDeviceResponse' <$>
                   (x .?> "UserConfirmationNecessary") <*>
                     (pure (fromEnum s)))

instance Hashable ConfirmDevice where

instance NFData ConfirmDevice where

instance ToHeaders ConfirmDevice where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSCognitoIdentityProviderService.ConfirmDevice" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ConfirmDevice where
        toJSON ConfirmDevice'{..}
          = object
              (catMaybes
                 [("DeviceSecretVerifierConfig" .=) <$>
                    _cdDeviceSecretVerifierConfig,
                  ("DeviceName" .=) <$> _cdDeviceName,
                  Just ("AccessToken" .= _cdAccessToken),
                  Just ("DeviceKey" .= _cdDeviceKey)])

instance ToPath ConfirmDevice where
        toPath = const "/"

instance ToQuery ConfirmDevice where
        toQuery = const mempty

-- | Confirms the device response.
--
--
--
-- /See:/ 'confirmDeviceResponse' smart constructor.
data ConfirmDeviceResponse = ConfirmDeviceResponse'
  { _cdrsUserConfirmationNecessary :: !(Maybe Bool)
  , _cdrsResponseStatus            :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ConfirmDeviceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdrsUserConfirmationNecessary' - Indicates whether the user confirmation is necessary to confirm the device response.
--
-- * 'cdrsResponseStatus' - -- | The response status code.
confirmDeviceResponse
    :: Int -- ^ 'cdrsResponseStatus'
    -> ConfirmDeviceResponse
confirmDeviceResponse pResponseStatus_ =
  ConfirmDeviceResponse'
    { _cdrsUserConfirmationNecessary = Nothing
    , _cdrsResponseStatus = pResponseStatus_
    }


-- | Indicates whether the user confirmation is necessary to confirm the device response.
cdrsUserConfirmationNecessary :: Lens' ConfirmDeviceResponse (Maybe Bool)
cdrsUserConfirmationNecessary = lens _cdrsUserConfirmationNecessary (\ s a -> s{_cdrsUserConfirmationNecessary = a})

-- | -- | The response status code.
cdrsResponseStatus :: Lens' ConfirmDeviceResponse Int
cdrsResponseStatus = lens _cdrsResponseStatus (\ s a -> s{_cdrsResponseStatus = a})

instance NFData ConfirmDeviceResponse where
