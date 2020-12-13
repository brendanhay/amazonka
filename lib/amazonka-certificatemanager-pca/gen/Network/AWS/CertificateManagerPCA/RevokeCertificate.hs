{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
-- ACM Private CA also writes revocation information to the audit report. For more information, see <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthorityAuditReport.html CreateCertificateAuthorityAuditReport> .
module Network.AWS.CertificateManagerPCA.RevokeCertificate
  ( -- * Creating a request
    RevokeCertificate (..),
    mkRevokeCertificate,

    -- ** Request lenses
    rcRevocationReason,
    rcCertificateSerial,
    rcCertificateAuthorityARN,

    -- * Destructuring the response
    RevokeCertificateResponse (..),
    mkRevokeCertificateResponse,
  )
where

import Network.AWS.CertificateManagerPCA.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRevokeCertificate' smart constructor.
data RevokeCertificate = RevokeCertificate'
  { -- | Specifies why you revoked the certificate.
    revocationReason :: RevocationReason,
    -- | Serial number of the certificate to be revoked. This must be in hexadecimal format. You can retrieve the serial number by calling <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_GetCertificate.html GetCertificate> with the Amazon Resource Name (ARN) of the certificate you want and the ARN of your private CA. The __GetCertificate__ action retrieves the certificate in the PEM format. You can use the following OpenSSL command to list the certificate in text format and copy the hexadecimal serial number.
    --
    -- @openssl x509 -in /file_path/ -text -noout@
    -- You can also copy the serial number from the console or use the <https://docs.aws.amazon.com/acm/latest/APIReference/API_DescribeCertificate.html DescribeCertificate> action in the /AWS Certificate Manager API Reference/ .
    certificateSerial :: Lude.Text,
    -- | Amazon Resource Name (ARN) of the private CA that issued the certificate to be revoked. This must be of the form:
    --
    -- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
    certificateAuthorityARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RevokeCertificate' with the minimum fields required to make a request.
--
-- * 'revocationReason' - Specifies why you revoked the certificate.
-- * 'certificateSerial' - Serial number of the certificate to be revoked. This must be in hexadecimal format. You can retrieve the serial number by calling <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_GetCertificate.html GetCertificate> with the Amazon Resource Name (ARN) of the certificate you want and the ARN of your private CA. The __GetCertificate__ action retrieves the certificate in the PEM format. You can use the following OpenSSL command to list the certificate in text format and copy the hexadecimal serial number.
--
-- @openssl x509 -in /file_path/ -text -noout@
-- You can also copy the serial number from the console or use the <https://docs.aws.amazon.com/acm/latest/APIReference/API_DescribeCertificate.html DescribeCertificate> action in the /AWS Certificate Manager API Reference/ .
-- * 'certificateAuthorityARN' - Amazon Resource Name (ARN) of the private CA that issued the certificate to be revoked. This must be of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
mkRevokeCertificate ::
  -- | 'revocationReason'
  RevocationReason ->
  -- | 'certificateSerial'
  Lude.Text ->
  -- | 'certificateAuthorityARN'
  Lude.Text ->
  RevokeCertificate
mkRevokeCertificate
  pRevocationReason_
  pCertificateSerial_
  pCertificateAuthorityARN_ =
    RevokeCertificate'
      { revocationReason = pRevocationReason_,
        certificateSerial = pCertificateSerial_,
        certificateAuthorityARN = pCertificateAuthorityARN_
      }

-- | Specifies why you revoked the certificate.
--
-- /Note:/ Consider using 'revocationReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcRevocationReason :: Lens.Lens' RevokeCertificate RevocationReason
rcRevocationReason = Lens.lens (revocationReason :: RevokeCertificate -> RevocationReason) (\s a -> s {revocationReason = a} :: RevokeCertificate)
{-# DEPRECATED rcRevocationReason "Use generic-lens or generic-optics with 'revocationReason' instead." #-}

-- | Serial number of the certificate to be revoked. This must be in hexadecimal format. You can retrieve the serial number by calling <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_GetCertificate.html GetCertificate> with the Amazon Resource Name (ARN) of the certificate you want and the ARN of your private CA. The __GetCertificate__ action retrieves the certificate in the PEM format. You can use the following OpenSSL command to list the certificate in text format and copy the hexadecimal serial number.
--
-- @openssl x509 -in /file_path/ -text -noout@
-- You can also copy the serial number from the console or use the <https://docs.aws.amazon.com/acm/latest/APIReference/API_DescribeCertificate.html DescribeCertificate> action in the /AWS Certificate Manager API Reference/ .
--
-- /Note:/ Consider using 'certificateSerial' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcCertificateSerial :: Lens.Lens' RevokeCertificate Lude.Text
rcCertificateSerial = Lens.lens (certificateSerial :: RevokeCertificate -> Lude.Text) (\s a -> s {certificateSerial = a} :: RevokeCertificate)
{-# DEPRECATED rcCertificateSerial "Use generic-lens or generic-optics with 'certificateSerial' instead." #-}

-- | Amazon Resource Name (ARN) of the private CA that issued the certificate to be revoked. This must be of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
--
-- /Note:/ Consider using 'certificateAuthorityARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcCertificateAuthorityARN :: Lens.Lens' RevokeCertificate Lude.Text
rcCertificateAuthorityARN = Lens.lens (certificateAuthorityARN :: RevokeCertificate -> Lude.Text) (\s a -> s {certificateAuthorityARN = a} :: RevokeCertificate)
{-# DEPRECATED rcCertificateAuthorityARN "Use generic-lens or generic-optics with 'certificateAuthorityARN' instead." #-}

instance Lude.AWSRequest RevokeCertificate where
  type Rs RevokeCertificate = RevokeCertificateResponse
  request = Req.postJSON certificateManagerPCAService
  response = Res.receiveNull RevokeCertificateResponse'

instance Lude.ToHeaders RevokeCertificate where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ACMPrivateCA.RevokeCertificate" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RevokeCertificate where
  toJSON RevokeCertificate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("RevocationReason" Lude..= revocationReason),
            Lude.Just ("CertificateSerial" Lude..= certificateSerial),
            Lude.Just
              ("CertificateAuthorityArn" Lude..= certificateAuthorityARN)
          ]
      )

instance Lude.ToPath RevokeCertificate where
  toPath = Lude.const "/"

instance Lude.ToQuery RevokeCertificate where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRevokeCertificateResponse' smart constructor.
data RevokeCertificateResponse = RevokeCertificateResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RevokeCertificateResponse' with the minimum fields required to make a request.
mkRevokeCertificateResponse ::
  RevokeCertificateResponse
mkRevokeCertificateResponse = RevokeCertificateResponse'
