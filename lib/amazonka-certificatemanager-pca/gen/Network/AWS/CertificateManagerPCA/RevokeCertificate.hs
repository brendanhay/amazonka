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
    rcCertificateAuthorityArn,
    rcCertificateSerial,
    rcRevocationReason,

    -- * Destructuring the response
    RevokeCertificateResponse (..),
    mkRevokeCertificateResponse,
  )
where

import qualified Network.AWS.CertificateManagerPCA.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRevokeCertificate' smart constructor.
data RevokeCertificate = RevokeCertificate'
  { -- | Amazon Resource Name (ARN) of the private CA that issued the certificate to be revoked. This must be of the form:
    --
    -- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
    certificateAuthorityArn :: Types.CertificateAuthorityArn,
    -- | Serial number of the certificate to be revoked. This must be in hexadecimal format. You can retrieve the serial number by calling <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_GetCertificate.html GetCertificate> with the Amazon Resource Name (ARN) of the certificate you want and the ARN of your private CA. The __GetCertificate__ action retrieves the certificate in the PEM format. You can use the following OpenSSL command to list the certificate in text format and copy the hexadecimal serial number.
    --
    -- @openssl x509 -in /file_path/ -text -noout@
    -- You can also copy the serial number from the console or use the <https://docs.aws.amazon.com/acm/latest/APIReference/API_DescribeCertificate.html DescribeCertificate> action in the /AWS Certificate Manager API Reference/ .
    certificateSerial :: Types.CertificateSerial,
    -- | Specifies why you revoked the certificate.
    revocationReason :: Types.RevocationReason
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RevokeCertificate' value with any optional fields omitted.
mkRevokeCertificate ::
  -- | 'certificateAuthorityArn'
  Types.CertificateAuthorityArn ->
  -- | 'certificateSerial'
  Types.CertificateSerial ->
  -- | 'revocationReason'
  Types.RevocationReason ->
  RevokeCertificate
mkRevokeCertificate
  certificateAuthorityArn
  certificateSerial
  revocationReason =
    RevokeCertificate'
      { certificateAuthorityArn,
        certificateSerial,
        revocationReason
      }

-- | Amazon Resource Name (ARN) of the private CA that issued the certificate to be revoked. This must be of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @
--
-- /Note:/ Consider using 'certificateAuthorityArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcCertificateAuthorityArn :: Lens.Lens' RevokeCertificate Types.CertificateAuthorityArn
rcCertificateAuthorityArn = Lens.field @"certificateAuthorityArn"
{-# DEPRECATED rcCertificateAuthorityArn "Use generic-lens or generic-optics with 'certificateAuthorityArn' instead." #-}

-- | Serial number of the certificate to be revoked. This must be in hexadecimal format. You can retrieve the serial number by calling <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_GetCertificate.html GetCertificate> with the Amazon Resource Name (ARN) of the certificate you want and the ARN of your private CA. The __GetCertificate__ action retrieves the certificate in the PEM format. You can use the following OpenSSL command to list the certificate in text format and copy the hexadecimal serial number.
--
-- @openssl x509 -in /file_path/ -text -noout@
-- You can also copy the serial number from the console or use the <https://docs.aws.amazon.com/acm/latest/APIReference/API_DescribeCertificate.html DescribeCertificate> action in the /AWS Certificate Manager API Reference/ .
--
-- /Note:/ Consider using 'certificateSerial' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcCertificateSerial :: Lens.Lens' RevokeCertificate Types.CertificateSerial
rcCertificateSerial = Lens.field @"certificateSerial"
{-# DEPRECATED rcCertificateSerial "Use generic-lens or generic-optics with 'certificateSerial' instead." #-}

-- | Specifies why you revoked the certificate.
--
-- /Note:/ Consider using 'revocationReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcRevocationReason :: Lens.Lens' RevokeCertificate Types.RevocationReason
rcRevocationReason = Lens.field @"revocationReason"
{-# DEPRECATED rcRevocationReason "Use generic-lens or generic-optics with 'revocationReason' instead." #-}

instance Core.FromJSON RevokeCertificate where
  toJSON RevokeCertificate {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("CertificateAuthorityArn" Core..= certificateAuthorityArn),
            Core.Just ("CertificateSerial" Core..= certificateSerial),
            Core.Just ("RevocationReason" Core..= revocationReason)
          ]
      )

instance Core.AWSRequest RevokeCertificate where
  type Rs RevokeCertificate = RevokeCertificateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "ACMPrivateCA.RevokeCertificate")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull RevokeCertificateResponse'

-- | /See:/ 'mkRevokeCertificateResponse' smart constructor.
data RevokeCertificateResponse = RevokeCertificateResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RevokeCertificateResponse' value with any optional fields omitted.
mkRevokeCertificateResponse ::
  RevokeCertificateResponse
mkRevokeCertificateResponse = RevokeCertificateResponse'
