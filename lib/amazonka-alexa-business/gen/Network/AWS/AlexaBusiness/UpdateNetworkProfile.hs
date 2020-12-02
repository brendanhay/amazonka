{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.UpdateNetworkProfile
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a network profile by the network profile ARN.
module Network.AWS.AlexaBusiness.UpdateNetworkProfile
  ( -- * Creating a Request
    updateNetworkProfile,
    UpdateNetworkProfile,

    -- * Request Lenses
    unpNetworkProfileName,
    unpCurrentPassword,
    unpNextPassword,
    unpDescription,
    unpTrustAnchors,
    unpCertificateAuthorityARN,
    unpNetworkProfileARN,

    -- * Destructuring the Response
    updateNetworkProfileResponse,
    UpdateNetworkProfileResponse,

    -- * Response Lenses
    unprsResponseStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateNetworkProfile' smart constructor.
data UpdateNetworkProfile = UpdateNetworkProfile'
  { _unpNetworkProfileName ::
      !(Maybe Text),
    _unpCurrentPassword :: !(Maybe (Sensitive Text)),
    _unpNextPassword :: !(Maybe (Sensitive Text)),
    _unpDescription :: !(Maybe Text),
    _unpTrustAnchors :: !(Maybe (List1 Text)),
    _unpCertificateAuthorityARN :: !(Maybe Text),
    _unpNetworkProfileARN :: !Text
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateNetworkProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'unpNetworkProfileName' - The name of the network profile associated with a device.
--
-- * 'unpCurrentPassword' - The current password of the Wi-Fi network.
--
-- * 'unpNextPassword' - The next, or subsequent, password of the Wi-Fi network. This password is asynchronously transmitted to the device and is used when the password of the network changes to NextPassword.
--
-- * 'unpDescription' - Detailed information about a device's network profile.
--
-- * 'unpTrustAnchors' - The root certificate(s) of your authentication server that will be installed on your devices and used to trust your authentication server during EAP negotiation.
--
-- * 'unpCertificateAuthorityARN' - The ARN of the Private Certificate Authority (PCA) created in AWS Certificate Manager (ACM). This is used to issue certificates to the devices.
--
-- * 'unpNetworkProfileARN' - The ARN of the network profile associated with a device.
updateNetworkProfile ::
  -- | 'unpNetworkProfileARN'
  Text ->
  UpdateNetworkProfile
updateNetworkProfile pNetworkProfileARN_ =
  UpdateNetworkProfile'
    { _unpNetworkProfileName = Nothing,
      _unpCurrentPassword = Nothing,
      _unpNextPassword = Nothing,
      _unpDescription = Nothing,
      _unpTrustAnchors = Nothing,
      _unpCertificateAuthorityARN = Nothing,
      _unpNetworkProfileARN = pNetworkProfileARN_
    }

-- | The name of the network profile associated with a device.
unpNetworkProfileName :: Lens' UpdateNetworkProfile (Maybe Text)
unpNetworkProfileName = lens _unpNetworkProfileName (\s a -> s {_unpNetworkProfileName = a})

-- | The current password of the Wi-Fi network.
unpCurrentPassword :: Lens' UpdateNetworkProfile (Maybe Text)
unpCurrentPassword = lens _unpCurrentPassword (\s a -> s {_unpCurrentPassword = a}) . mapping _Sensitive

-- | The next, or subsequent, password of the Wi-Fi network. This password is asynchronously transmitted to the device and is used when the password of the network changes to NextPassword.
unpNextPassword :: Lens' UpdateNetworkProfile (Maybe Text)
unpNextPassword = lens _unpNextPassword (\s a -> s {_unpNextPassword = a}) . mapping _Sensitive

-- | Detailed information about a device's network profile.
unpDescription :: Lens' UpdateNetworkProfile (Maybe Text)
unpDescription = lens _unpDescription (\s a -> s {_unpDescription = a})

-- | The root certificate(s) of your authentication server that will be installed on your devices and used to trust your authentication server during EAP negotiation.
unpTrustAnchors :: Lens' UpdateNetworkProfile (Maybe (NonEmpty Text))
unpTrustAnchors = lens _unpTrustAnchors (\s a -> s {_unpTrustAnchors = a}) . mapping _List1

-- | The ARN of the Private Certificate Authority (PCA) created in AWS Certificate Manager (ACM). This is used to issue certificates to the devices.
unpCertificateAuthorityARN :: Lens' UpdateNetworkProfile (Maybe Text)
unpCertificateAuthorityARN = lens _unpCertificateAuthorityARN (\s a -> s {_unpCertificateAuthorityARN = a})

-- | The ARN of the network profile associated with a device.
unpNetworkProfileARN :: Lens' UpdateNetworkProfile Text
unpNetworkProfileARN = lens _unpNetworkProfileARN (\s a -> s {_unpNetworkProfileARN = a})

instance AWSRequest UpdateNetworkProfile where
  type Rs UpdateNetworkProfile = UpdateNetworkProfileResponse
  request = postJSON alexaBusiness
  response =
    receiveEmpty
      (\s h x -> UpdateNetworkProfileResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateNetworkProfile

instance NFData UpdateNetworkProfile

instance ToHeaders UpdateNetworkProfile where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AlexaForBusiness.UpdateNetworkProfile" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateNetworkProfile where
  toJSON UpdateNetworkProfile' {..} =
    object
      ( catMaybes
          [ ("NetworkProfileName" .=) <$> _unpNetworkProfileName,
            ("CurrentPassword" .=) <$> _unpCurrentPassword,
            ("NextPassword" .=) <$> _unpNextPassword,
            ("Description" .=) <$> _unpDescription,
            ("TrustAnchors" .=) <$> _unpTrustAnchors,
            ("CertificateAuthorityArn" .=) <$> _unpCertificateAuthorityARN,
            Just ("NetworkProfileArn" .= _unpNetworkProfileARN)
          ]
      )

instance ToPath UpdateNetworkProfile where
  toPath = const "/"

instance ToQuery UpdateNetworkProfile where
  toQuery = const mempty

-- | /See:/ 'updateNetworkProfileResponse' smart constructor.
newtype UpdateNetworkProfileResponse = UpdateNetworkProfileResponse'
  { _unprsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateNetworkProfileResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'unprsResponseStatus' - -- | The response status code.
updateNetworkProfileResponse ::
  -- | 'unprsResponseStatus'
  Int ->
  UpdateNetworkProfileResponse
updateNetworkProfileResponse pResponseStatus_ =
  UpdateNetworkProfileResponse'
    { _unprsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
unprsResponseStatus :: Lens' UpdateNetworkProfileResponse Int
unprsResponseStatus = lens _unprsResponseStatus (\s a -> s {_unprsResponseStatus = a})

instance NFData UpdateNetworkProfileResponse
