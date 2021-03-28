{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.GetCertificateAuthorityCsr
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the certificate signing request (CSR) for your private certificate authority (CA). The CSR is created when you call the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> action. Sign the CSR with your ACM Private CA-hosted or on-premises root or subordinate CA. Then import the signed certificate back into ACM Private CA by calling the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_ImportCertificateAuthorityCertificate.html ImportCertificateAuthorityCertificate> action. The CSR is returned as a base64 PEM-encoded string. 
module Network.AWS.CertificateManagerPCA.GetCertificateAuthorityCsr
    (
    -- * Creating a request
      GetCertificateAuthorityCsr (..)
    , mkGetCertificateAuthorityCsr
    -- ** Request lenses
    , gcacCertificateAuthorityArn

    -- * Destructuring the response
    , GetCertificateAuthorityCsrResponse (..)
    , mkGetCertificateAuthorityCsrResponse
    -- ** Response lenses
    , gcacrrsCsr
    , gcacrrsResponseStatus
    ) where

import qualified Network.AWS.CertificateManagerPCA.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetCertificateAuthorityCsr' smart constructor.
newtype GetCertificateAuthorityCsr = GetCertificateAuthorityCsr'
  { certificateAuthorityArn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) that was returned when you called the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> action. This must be of the form: 
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetCertificateAuthorityCsr' value with any optional fields omitted.
mkGetCertificateAuthorityCsr
    :: Types.Arn -- ^ 'certificateAuthorityArn'
    -> GetCertificateAuthorityCsr
mkGetCertificateAuthorityCsr certificateAuthorityArn
  = GetCertificateAuthorityCsr'{certificateAuthorityArn}

-- | The Amazon Resource Name (ARN) that was returned when you called the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> action. This must be of the form: 
--
-- @arn:aws:acm-pca:/region/ :/account/ :certificate-authority//12345678-1234-1234-1234-123456789012/ @ 
--
-- /Note:/ Consider using 'certificateAuthorityArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcacCertificateAuthorityArn :: Lens.Lens' GetCertificateAuthorityCsr Types.Arn
gcacCertificateAuthorityArn = Lens.field @"certificateAuthorityArn"
{-# INLINEABLE gcacCertificateAuthorityArn #-}
{-# DEPRECATED certificateAuthorityArn "Use generic-lens or generic-optics with 'certificateAuthorityArn' instead"  #-}

instance Core.ToQuery GetCertificateAuthorityCsr where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetCertificateAuthorityCsr where
        toHeaders GetCertificateAuthorityCsr{..}
          = Core.pure
              ("X-Amz-Target", "ACMPrivateCA.GetCertificateAuthorityCsr")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetCertificateAuthorityCsr where
        toJSON GetCertificateAuthorityCsr{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("CertificateAuthorityArn" Core..= certificateAuthorityArn)])

instance Core.AWSRequest GetCertificateAuthorityCsr where
        type Rs GetCertificateAuthorityCsr =
             GetCertificateAuthorityCsrResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetCertificateAuthorityCsrResponse' Core.<$>
                   (x Core..:? "Csr") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetCertificateAuthorityCsrResponse' smart constructor.
data GetCertificateAuthorityCsrResponse = GetCertificateAuthorityCsrResponse'
  { csr :: Core.Maybe Types.CsrBody
    -- ^ The base64 PEM-encoded certificate signing request (CSR) for your private CA certificate.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCertificateAuthorityCsrResponse' value with any optional fields omitted.
mkGetCertificateAuthorityCsrResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetCertificateAuthorityCsrResponse
mkGetCertificateAuthorityCsrResponse responseStatus
  = GetCertificateAuthorityCsrResponse'{csr = Core.Nothing,
                                        responseStatus}

-- | The base64 PEM-encoded certificate signing request (CSR) for your private CA certificate.
--
-- /Note:/ Consider using 'csr' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcacrrsCsr :: Lens.Lens' GetCertificateAuthorityCsrResponse (Core.Maybe Types.CsrBody)
gcacrrsCsr = Lens.field @"csr"
{-# INLINEABLE gcacrrsCsr #-}
{-# DEPRECATED csr "Use generic-lens or generic-optics with 'csr' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcacrrsResponseStatus :: Lens.Lens' GetCertificateAuthorityCsrResponse Core.Int
gcacrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcacrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
