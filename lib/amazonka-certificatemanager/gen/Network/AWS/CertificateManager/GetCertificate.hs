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
-- Module      : Network.AWS.CertificateManager.GetCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an Amazon-issued certificate and its certificate chain. The chain consists of the certificate of the issuing CA and the intermediate certificates of any other subordinate CAs. All of the certificates are base64 encoded. You can use <https://wiki.openssl.org/index.php/Command_Line_Utilities OpenSSL> to decode the certificates and inspect individual fields.
module Network.AWS.CertificateManager.GetCertificate
  ( -- * Creating a Request
    getCertificate,
    GetCertificate,

    -- * Request Lenses
    gcCertificateARN,

    -- * Destructuring the Response
    getCertificateResponse,
    GetCertificateResponse,

    -- * Response Lenses
    gcrsCertificate,
    gcrsCertificateChain,
    gcrsResponseStatus,
  )
where

import Network.AWS.CertificateManager.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getCertificate' smart constructor.
newtype GetCertificate = GetCertificate' {_gcCertificateARN :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcCertificateARN' - String that contains a certificate ARN in the following format: @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@  For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
getCertificate ::
  -- | 'gcCertificateARN'
  Text ->
  GetCertificate
getCertificate pCertificateARN_ =
  GetCertificate' {_gcCertificateARN = pCertificateARN_}

-- | String that contains a certificate ARN in the following format: @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@  For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
gcCertificateARN :: Lens' GetCertificate Text
gcCertificateARN = lens _gcCertificateARN (\s a -> s {_gcCertificateARN = a})

instance AWSRequest GetCertificate where
  type Rs GetCertificate = GetCertificateResponse
  request = postJSON certificateManager
  response =
    receiveJSON
      ( \s h x ->
          GetCertificateResponse'
            <$> (x .?> "Certificate")
            <*> (x .?> "CertificateChain")
            <*> (pure (fromEnum s))
      )

instance Hashable GetCertificate

instance NFData GetCertificate

instance ToHeaders GetCertificate where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("CertificateManager.GetCertificate" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetCertificate where
  toJSON GetCertificate' {..} =
    object (catMaybes [Just ("CertificateArn" .= _gcCertificateARN)])

instance ToPath GetCertificate where
  toPath = const "/"

instance ToQuery GetCertificate where
  toQuery = const mempty

-- | /See:/ 'getCertificateResponse' smart constructor.
data GetCertificateResponse = GetCertificateResponse'
  { _gcrsCertificate ::
      !(Maybe Text),
    _gcrsCertificateChain :: !(Maybe Text),
    _gcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetCertificateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcrsCertificate' - The ACM-issued certificate corresponding to the ARN specified as input.
--
-- * 'gcrsCertificateChain' - Certificates forming the requested certificate's chain of trust. The chain consists of the certificate of the issuing CA and the intermediate certificates of any other subordinate CAs.
--
-- * 'gcrsResponseStatus' - -- | The response status code.
getCertificateResponse ::
  -- | 'gcrsResponseStatus'
  Int ->
  GetCertificateResponse
getCertificateResponse pResponseStatus_ =
  GetCertificateResponse'
    { _gcrsCertificate = Nothing,
      _gcrsCertificateChain = Nothing,
      _gcrsResponseStatus = pResponseStatus_
    }

-- | The ACM-issued certificate corresponding to the ARN specified as input.
gcrsCertificate :: Lens' GetCertificateResponse (Maybe Text)
gcrsCertificate = lens _gcrsCertificate (\s a -> s {_gcrsCertificate = a})

-- | Certificates forming the requested certificate's chain of trust. The chain consists of the certificate of the issuing CA and the intermediate certificates of any other subordinate CAs.
gcrsCertificateChain :: Lens' GetCertificateResponse (Maybe Text)
gcrsCertificateChain = lens _gcrsCertificateChain (\s a -> s {_gcrsCertificateChain = a})

-- | -- | The response status code.
gcrsResponseStatus :: Lens' GetCertificateResponse Int
gcrsResponseStatus = lens _gcrsResponseStatus (\s a -> s {_gcrsResponseStatus = a})

instance NFData GetCertificateResponse
