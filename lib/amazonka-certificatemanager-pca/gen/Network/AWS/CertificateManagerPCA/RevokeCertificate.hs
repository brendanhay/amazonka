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
-- Module      : Network.AWS.CertificateManagerPCA.RevokeCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes a certificate that was issued inside ACM Private CA. If you enable a certificate revocation list (CRL) when you create or update your private CA, information about the revoked certificates will be included in the CRL. ACM Private CA writes the CRL to an S3 bucket that you specify. A CRL is typically updated approximately 30 minutes after a certificate is revoked. If for any reason the CRL update fails, ACM Private CA attempts makes further attempts every 15 minutes. With Amazon CloudWatch, you can create alarms for the metrics @CRLGenerated@ and @MisconfiguredCRLBucket@ . For more information, see <https://docs.aws.amazon.com/acm-pca/latest/userguide/PcaCloudWatch.html Supported CloudWatch Metrics> .
--
--
-- ACM Private CA also writes revocation information to the audit report. For more information, see <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthorityAuditReport.html CreateCertificateAuthorityAuditReport> .
module Network.AWS.CertificateManagerPCA.RevokeCertificate
  ( -- * Creating a Request
    revokeCertificate,
    RevokeCertificate,

    -- * Request Lenses
    rcCertificateAuthorityARN,
    rcCertificateSerial,
    rcRevocationReason,

    -- * Destructuring the Response
    revokeCertificateResponse,
    RevokeCertificateResponse,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'revokeCertificate' smart constructor.
data RevokeCertificate = RevokeCertificate'
  { _rcCertificateAuthorityARN ::
      !Text,
    _rcCertificateSerial :: !Text,
    _rcRevocationReason :: !RevocationReason
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RevokeCertificate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcCertificateAuthorityARN' - Amazon Resource Name (ARN) of the private CA that issued the certificate to be revoked. This must be of the form: @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
--
-- * 'rcCertificateSerial' - Serial number of the certificate to be revoked. This must be in hexadecimal format. You can retrieve the serial number by calling <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_GetCertificate.html GetCertificate> with the Amazon Resource Name (ARN) of the certificate you want and the ARN of your private CA. The __GetCertificate__ action retrieves the certificate in the PEM format. You can use the following OpenSSL command to list the certificate in text format and copy the hexadecimal serial number.  @openssl x509 -in /file_path/ -text -noout@  You can also copy the serial number from the console or use the <https://docs.aws.amazon.com/acm/latest/APIReference/API_DescribeCertificate.html DescribeCertificate> action in the /AWS Certificate Manager API Reference/ .
--
-- * 'rcRevocationReason' - Specifies why you revoked the certificate.
revokeCertificate ::
  -- | 'rcCertificateAuthorityARN'
  Text ->
  -- | 'rcCertificateSerial'
  Text ->
  -- | 'rcRevocationReason'
  RevocationReason ->
  RevokeCertificate
revokeCertificate
  pCertificateAuthorityARN_
  pCertificateSerial_
  pRevocationReason_ =
    RevokeCertificate'
      { _rcCertificateAuthorityARN =
          pCertificateAuthorityARN_,
        _rcCertificateSerial = pCertificateSerial_,
        _rcRevocationReason = pRevocationReason_
      }

-- | Amazon Resource Name (ARN) of the private CA that issued the certificate to be revoked. This must be of the form: @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
rcCertificateAuthorityARN :: Lens' RevokeCertificate Text
rcCertificateAuthorityARN = lens _rcCertificateAuthorityARN (\s a -> s {_rcCertificateAuthorityARN = a})

-- | Serial number of the certificate to be revoked. This must be in hexadecimal format. You can retrieve the serial number by calling <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_GetCertificate.html GetCertificate> with the Amazon Resource Name (ARN) of the certificate you want and the ARN of your private CA. The __GetCertificate__ action retrieves the certificate in the PEM format. You can use the following OpenSSL command to list the certificate in text format and copy the hexadecimal serial number.  @openssl x509 -in /file_path/ -text -noout@  You can also copy the serial number from the console or use the <https://docs.aws.amazon.com/acm/latest/APIReference/API_DescribeCertificate.html DescribeCertificate> action in the /AWS Certificate Manager API Reference/ .
rcCertificateSerial :: Lens' RevokeCertificate Text
rcCertificateSerial = lens _rcCertificateSerial (\s a -> s {_rcCertificateSerial = a})

-- | Specifies why you revoked the certificate.
rcRevocationReason :: Lens' RevokeCertificate RevocationReason
rcRevocationReason = lens _rcRevocationReason (\s a -> s {_rcRevocationReason = a})

instance AWSRequest RevokeCertificate where
  type Rs RevokeCertificate = RevokeCertificateResponse
  request = postJSON certificateManagerPCA
  response = receiveNull RevokeCertificateResponse'

instance Hashable RevokeCertificate

instance NFData RevokeCertificate

instance ToHeaders RevokeCertificate where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("ACMPrivateCA.RevokeCertificate" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON RevokeCertificate where
  toJSON RevokeCertificate' {..} =
    object
      ( catMaybes
          [ Just ("CertificateAuthorityArn" .= _rcCertificateAuthorityARN),
            Just ("CertificateSerial" .= _rcCertificateSerial),
            Just ("RevocationReason" .= _rcRevocationReason)
          ]
      )

instance ToPath RevokeCertificate where
  toPath = const "/"

instance ToQuery RevokeCertificate where
  toQuery = const mempty

-- | /See:/ 'revokeCertificateResponse' smart constructor.
data RevokeCertificateResponse = RevokeCertificateResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RevokeCertificateResponse' with the minimum fields required to make a request.
revokeCertificateResponse ::
  RevokeCertificateResponse
revokeCertificateResponse = RevokeCertificateResponse'

instance NFData RevokeCertificateResponse
