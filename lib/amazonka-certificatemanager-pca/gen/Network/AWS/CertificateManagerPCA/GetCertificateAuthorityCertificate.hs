{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.GetCertificateAuthorityCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the certificate and certificate chain for your private certificate authority (CA) or one that has been shared with you. Both the certificate and the chain are base64 PEM-encoded. The chain does not include the CA certificate. Each certificate in the chain signs the one before it. 
module Network.AWS.CertificateManagerPCA.GetCertificateAuthorityCertificate
    (
    -- * Creating a request
      GetCertificateAuthorityCertificate (..)
    , mkGetCertificateAuthorityCertificate
    -- ** Request lenses
    , gCertificateAuthorityArn

    -- * Destructuring the response
    , GetCertificateAuthorityCertificateResponse (..)
    , mkGetCertificateAuthorityCertificateResponse
    -- ** Response lenses
    , grsCertificate
    , grsCertificateChain
    , grsResponseStatus
    ) where

import qualified Network.AWS.CertificateManagerPCA.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetCertificateAuthorityCertificate' smart constructor.
newtype GetCertificateAuthorityCertificate = GetCertificateAuthorityCertificate'
  { certificateAuthorityArn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) of your private CA. This is of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetCertificateAuthorityCertificate' value with any optional fields omitted.
mkGetCertificateAuthorityCertificate
    :: Types.Arn -- ^ 'certificateAuthorityArn'
    -> GetCertificateAuthorityCertificate
mkGetCertificateAuthorityCertificate certificateAuthorityArn
  = GetCertificateAuthorityCertificate'{certificateAuthorityArn}

-- | The Amazon Resource Name (ARN) of your private CA. This is of the form:
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ . 
--
-- /Note:/ Consider using 'certificateAuthorityArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gCertificateAuthorityArn :: Lens.Lens' GetCertificateAuthorityCertificate Types.Arn
gCertificateAuthorityArn = Lens.field @"certificateAuthorityArn"
{-# INLINEABLE gCertificateAuthorityArn #-}
{-# DEPRECATED certificateAuthorityArn "Use generic-lens or generic-optics with 'certificateAuthorityArn' instead"  #-}

instance Core.ToQuery GetCertificateAuthorityCertificate where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetCertificateAuthorityCertificate where
        toHeaders GetCertificateAuthorityCertificate{..}
          = Core.pure
              ("X-Amz-Target", "ACMPrivateCA.GetCertificateAuthorityCertificate")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetCertificateAuthorityCertificate where
        toJSON GetCertificateAuthorityCertificate{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("CertificateAuthorityArn" Core..= certificateAuthorityArn)])

instance Core.AWSRequest GetCertificateAuthorityCertificate where
        type Rs GetCertificateAuthorityCertificate =
             GetCertificateAuthorityCertificateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetCertificateAuthorityCertificateResponse' Core.<$>
                   (x Core..:? "Certificate") Core.<*> x Core..:? "CertificateChain"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetCertificateAuthorityCertificateResponse' smart constructor.
data GetCertificateAuthorityCertificateResponse = GetCertificateAuthorityCertificateResponse'
  { certificate :: Core.Maybe Types.Certificate
    -- ^ Base64-encoded certificate authority (CA) certificate.
  , certificateChain :: Core.Maybe Types.CertificateChain
    -- ^ Base64-encoded certificate chain that includes any intermediate certificates and chains up to root on-premises certificate that you used to sign your private CA certificate. The chain does not include your private CA certificate. If this is a root CA, the value will be null.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCertificateAuthorityCertificateResponse' value with any optional fields omitted.
mkGetCertificateAuthorityCertificateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetCertificateAuthorityCertificateResponse
mkGetCertificateAuthorityCertificateResponse responseStatus
  = GetCertificateAuthorityCertificateResponse'{certificate =
                                                  Core.Nothing,
                                                certificateChain = Core.Nothing, responseStatus}

-- | Base64-encoded certificate authority (CA) certificate.
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsCertificate :: Lens.Lens' GetCertificateAuthorityCertificateResponse (Core.Maybe Types.Certificate)
grsCertificate = Lens.field @"certificate"
{-# INLINEABLE grsCertificate #-}
{-# DEPRECATED certificate "Use generic-lens or generic-optics with 'certificate' instead"  #-}

-- | Base64-encoded certificate chain that includes any intermediate certificates and chains up to root on-premises certificate that you used to sign your private CA certificate. The chain does not include your private CA certificate. If this is a root CA, the value will be null.
--
-- /Note:/ Consider using 'certificateChain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsCertificateChain :: Lens.Lens' GetCertificateAuthorityCertificateResponse (Core.Maybe Types.CertificateChain)
grsCertificateChain = Lens.field @"certificateChain"
{-# INLINEABLE grsCertificateChain #-}
{-# DEPRECATED certificateChain "Use generic-lens or generic-optics with 'certificateChain' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsResponseStatus :: Lens.Lens' GetCertificateAuthorityCertificateResponse Core.Int
grsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
