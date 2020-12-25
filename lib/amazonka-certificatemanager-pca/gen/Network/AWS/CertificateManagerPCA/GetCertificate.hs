{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.GetCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a certificate from your private CA or one that has been shared with you. The ARN of the certificate is returned when you call the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_IssueCertificate.html IssueCertificate> action. You must specify both the ARN of your private CA and the ARN of the issued certificate when calling the __GetCertificate__ action. You can retrieve the certificate if it is in the __ISSUED__ state. You can call the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthorityAuditReport.html CreateCertificateAuthorityAuditReport> action to create a report that contains information about all of the certificates issued and revoked by your private CA.
module Network.AWS.CertificateManagerPCA.GetCertificate
  ( -- * Creating a request
    GetCertificate (..),
    mkGetCertificate,

    -- ** Request lenses
    gcCertificateAuthorityArn,
    gcCertificateArn,

    -- * Destructuring the response
    GetCertificateResponse (..),
    mkGetCertificateResponse,

    -- ** Response lenses
    gcrrsCertificate,
    gcrrsCertificateChain,
    gcrrsResponseStatus,
  )
where

import qualified Network.AWS.CertificateManagerPCA.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetCertificate' smart constructor.
data GetCertificate = GetCertificate'
  { -- | The Amazon Resource Name (ARN) that was returned when you called <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> . This must be of the form:
    --
    -- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
    certificateAuthorityArn :: Types.CertificateAuthorityArn,
    -- | The ARN of the issued certificate. The ARN contains the certificate serial number and must be in the following form:
    --
    -- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ /certificate//286535153982981100925020015808220737245/ @
    certificateArn :: Types.CertificateArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCertificate' value with any optional fields omitted.
mkGetCertificate ::
  -- | 'certificateAuthorityArn'
  Types.CertificateAuthorityArn ->
  -- | 'certificateArn'
  Types.CertificateArn ->
  GetCertificate
mkGetCertificate certificateAuthorityArn certificateArn =
  GetCertificate' {certificateAuthorityArn, certificateArn}

-- | The Amazon Resource Name (ARN) that was returned when you called <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> . This must be of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ .
--
-- /Note:/ Consider using 'certificateAuthorityArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcCertificateAuthorityArn :: Lens.Lens' GetCertificate Types.CertificateAuthorityArn
gcCertificateAuthorityArn = Lens.field @"certificateAuthorityArn"
{-# DEPRECATED gcCertificateAuthorityArn "Use generic-lens or generic-optics with 'certificateAuthorityArn' instead." #-}

-- | The ARN of the issued certificate. The ARN contains the certificate serial number and must be in the following form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ /certificate//286535153982981100925020015808220737245/ @
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcCertificateArn :: Lens.Lens' GetCertificate Types.CertificateArn
gcCertificateArn = Lens.field @"certificateArn"
{-# DEPRECATED gcCertificateArn "Use generic-lens or generic-optics with 'certificateArn' instead." #-}

instance Core.FromJSON GetCertificate where
  toJSON GetCertificate {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("CertificateAuthorityArn" Core..= certificateAuthorityArn),
            Core.Just ("CertificateArn" Core..= certificateArn)
          ]
      )

instance Core.AWSRequest GetCertificate where
  type Rs GetCertificate = GetCertificateResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "ACMPrivateCA.GetCertificate")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCertificateResponse'
            Core.<$> (x Core..:? "Certificate")
            Core.<*> (x Core..:? "CertificateChain")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetCertificateResponse' smart constructor.
data GetCertificateResponse = GetCertificateResponse'
  { -- | The base64 PEM-encoded certificate specified by the @CertificateArn@ parameter.
    certificate :: Core.Maybe Types.Certificate,
    -- | The base64 PEM-encoded certificate chain that chains up to the on-premises root CA certificate that you used to sign your private CA certificate.
    certificateChain :: Core.Maybe Types.CertificateChain,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCertificateResponse' value with any optional fields omitted.
mkGetCertificateResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetCertificateResponse
mkGetCertificateResponse responseStatus =
  GetCertificateResponse'
    { certificate = Core.Nothing,
      certificateChain = Core.Nothing,
      responseStatus
    }

-- | The base64 PEM-encoded certificate specified by the @CertificateArn@ parameter.
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsCertificate :: Lens.Lens' GetCertificateResponse (Core.Maybe Types.Certificate)
gcrrsCertificate = Lens.field @"certificate"
{-# DEPRECATED gcrrsCertificate "Use generic-lens or generic-optics with 'certificate' instead." #-}

-- | The base64 PEM-encoded certificate chain that chains up to the on-premises root CA certificate that you used to sign your private CA certificate.
--
-- /Note:/ Consider using 'certificateChain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsCertificateChain :: Lens.Lens' GetCertificateResponse (Core.Maybe Types.CertificateChain)
gcrrsCertificateChain = Lens.field @"certificateChain"
{-# DEPRECATED gcrrsCertificateChain "Use generic-lens or generic-optics with 'certificateChain' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsResponseStatus :: Lens.Lens' GetCertificateResponse Core.Int
gcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
