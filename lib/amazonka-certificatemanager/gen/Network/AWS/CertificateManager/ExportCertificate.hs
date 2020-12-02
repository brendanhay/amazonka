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
-- Module      : Network.AWS.CertificateManager.ExportCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports a private certificate issued by a private certificate authority (CA) for use anywhere. The exported file contains the certificate, the certificate chain, and the encrypted private 2048-bit RSA key associated with the public key that is embedded in the certificate. For security, you must assign a passphrase for the private key when exporting it.
--
--
-- For information about exporting and formatting a certificate using the ACM console or CLI, see <https://docs.aws.amazon.com/acm/latest/userguide/gs-acm-export-private.html Export a Private Certificate> .
module Network.AWS.CertificateManager.ExportCertificate
  ( -- * Creating a Request
    exportCertificate,
    ExportCertificate,

    -- * Request Lenses
    ecCertificateARN,
    ecPassphrase,

    -- * Destructuring the Response
    exportCertificateResponse,
    ExportCertificateResponse,

    -- * Response Lenses
    ecrsPrivateKey,
    ecrsCertificate,
    ecrsCertificateChain,
    ecrsResponseStatus,
  )
where

import Network.AWS.CertificateManager.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'exportCertificate' smart constructor.
data ExportCertificate = ExportCertificate'
  { _ecCertificateARN ::
      !Text,
    _ecPassphrase :: !(Sensitive Base64)
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExportCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecCertificateARN' - An Amazon Resource Name (ARN) of the issued certificate. This must be of the form: @arn:aws:acm:region:account:certificate/12345678-1234-1234-1234-123456789012@
--
-- * 'ecPassphrase' - Passphrase to associate with the encrypted exported private key. If you want to later decrypt the private key, you must have the passphrase. You can use the following OpenSSL command to decrypt a private key:  @openssl rsa -in encrypted_key.pem -out decrypted_key.pem@ -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
exportCertificate ::
  -- | 'ecCertificateARN'
  Text ->
  -- | 'ecPassphrase'
  ByteString ->
  ExportCertificate
exportCertificate pCertificateARN_ pPassphrase_ =
  ExportCertificate'
    { _ecCertificateARN = pCertificateARN_,
      _ecPassphrase = _Sensitive . _Base64 # pPassphrase_
    }

-- | An Amazon Resource Name (ARN) of the issued certificate. This must be of the form: @arn:aws:acm:region:account:certificate/12345678-1234-1234-1234-123456789012@
ecCertificateARN :: Lens' ExportCertificate Text
ecCertificateARN = lens _ecCertificateARN (\s a -> s {_ecCertificateARN = a})

-- | Passphrase to associate with the encrypted exported private key. If you want to later decrypt the private key, you must have the passphrase. You can use the following OpenSSL command to decrypt a private key:  @openssl rsa -in encrypted_key.pem -out decrypted_key.pem@ -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data. The underlying isomorphism will encode to Base64 representation during serialisation, and decode from Base64 representation during deserialisation. This 'Lens' accepts and returns only raw unencoded data.
ecPassphrase :: Lens' ExportCertificate ByteString
ecPassphrase = lens _ecPassphrase (\s a -> s {_ecPassphrase = a}) . _Sensitive . _Base64

instance AWSRequest ExportCertificate where
  type Rs ExportCertificate = ExportCertificateResponse
  request = postJSON certificateManager
  response =
    receiveJSON
      ( \s h x ->
          ExportCertificateResponse'
            <$> (x .?> "PrivateKey")
            <*> (x .?> "Certificate")
            <*> (x .?> "CertificateChain")
            <*> (pure (fromEnum s))
      )

instance Hashable ExportCertificate

instance NFData ExportCertificate

instance ToHeaders ExportCertificate where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("CertificateManager.ExportCertificate" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ExportCertificate where
  toJSON ExportCertificate' {..} =
    object
      ( catMaybes
          [ Just ("CertificateArn" .= _ecCertificateARN),
            Just ("Passphrase" .= _ecPassphrase)
          ]
      )

instance ToPath ExportCertificate where
  toPath = const "/"

instance ToQuery ExportCertificate where
  toQuery = const mempty

-- | /See:/ 'exportCertificateResponse' smart constructor.
data ExportCertificateResponse = ExportCertificateResponse'
  { _ecrsPrivateKey ::
      !(Maybe (Sensitive Text)),
    _ecrsCertificate :: !(Maybe Text),
    _ecrsCertificateChain :: !(Maybe Text),
    _ecrsResponseStatus :: !Int
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExportCertificateResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ecrsPrivateKey' - The encrypted private key associated with the public key in the certificate. The key is output in PKCS #8 format and is base64 PEM-encoded.
--
-- * 'ecrsCertificate' - The base64 PEM-encoded certificate.
--
-- * 'ecrsCertificateChain' - The base64 PEM-encoded certificate chain. This does not include the certificate that you are exporting.
--
-- * 'ecrsResponseStatus' - -- | The response status code.
exportCertificateResponse ::
  -- | 'ecrsResponseStatus'
  Int ->
  ExportCertificateResponse
exportCertificateResponse pResponseStatus_ =
  ExportCertificateResponse'
    { _ecrsPrivateKey = Nothing,
      _ecrsCertificate = Nothing,
      _ecrsCertificateChain = Nothing,
      _ecrsResponseStatus = pResponseStatus_
    }

-- | The encrypted private key associated with the public key in the certificate. The key is output in PKCS #8 format and is base64 PEM-encoded.
ecrsPrivateKey :: Lens' ExportCertificateResponse (Maybe Text)
ecrsPrivateKey = lens _ecrsPrivateKey (\s a -> s {_ecrsPrivateKey = a}) . mapping _Sensitive

-- | The base64 PEM-encoded certificate.
ecrsCertificate :: Lens' ExportCertificateResponse (Maybe Text)
ecrsCertificate = lens _ecrsCertificate (\s a -> s {_ecrsCertificate = a})

-- | The base64 PEM-encoded certificate chain. This does not include the certificate that you are exporting.
ecrsCertificateChain :: Lens' ExportCertificateResponse (Maybe Text)
ecrsCertificateChain = lens _ecrsCertificateChain (\s a -> s {_ecrsCertificateChain = a})

-- | -- | The response status code.
ecrsResponseStatus :: Lens' ExportCertificateResponse Int
ecrsResponseStatus = lens _ecrsResponseStatus (\s a -> s {_ecrsResponseStatus = a})

instance NFData ExportCertificateResponse
