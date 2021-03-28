{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetCertificate (..)
    , mkGetCertificate
    -- ** Request lenses
    , gcCertificateArn

    -- * Destructuring the response
    , GetCertificateResponse (..)
    , mkGetCertificateResponse
    -- ** Response lenses
    , gcrrsCertificate
    , gcrrsCertificateChain
    , gcrrsResponseStatus
    ) where

import qualified Network.AWS.CertificateManager.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetCertificate' smart constructor.
newtype GetCertificate = GetCertificate'
  { certificateArn :: Types.CertificateArn
    -- ^ String that contains a certificate ARN in the following format:
--
-- @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@ 
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetCertificate' value with any optional fields omitted.
mkGetCertificate
    :: Types.CertificateArn -- ^ 'certificateArn'
    -> GetCertificate
mkGetCertificate certificateArn = GetCertificate'{certificateArn}

-- | String that contains a certificate ARN in the following format:
--
-- @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@ 
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcCertificateArn :: Lens.Lens' GetCertificate Types.CertificateArn
gcCertificateArn = Lens.field @"certificateArn"
{-# INLINEABLE gcCertificateArn #-}
{-# DEPRECATED certificateArn "Use generic-lens or generic-optics with 'certificateArn' instead"  #-}

instance Core.ToQuery GetCertificate where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetCertificate where
        toHeaders GetCertificate{..}
          = Core.pure ("X-Amz-Target", "CertificateManager.GetCertificate")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetCertificate where
        toJSON GetCertificate{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("CertificateArn" Core..= certificateArn)])

instance Core.AWSRequest GetCertificate where
        type Rs GetCertificate = GetCertificateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetCertificateResponse' Core.<$>
                   (x Core..:? "Certificate") Core.<*> x Core..:? "CertificateChain"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetCertificateResponse' smart constructor.
data GetCertificateResponse = GetCertificateResponse'
  { certificate :: Core.Maybe Types.Certificate
    -- ^ The ACM-issued certificate corresponding to the ARN specified as input.
  , certificateChain :: Core.Maybe Types.CertificateChain
    -- ^ Certificates forming the requested certificate's chain of trust. The chain consists of the certificate of the issuing CA and the intermediate certificates of any other subordinate CAs. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetCertificateResponse' value with any optional fields omitted.
mkGetCertificateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetCertificateResponse
mkGetCertificateResponse responseStatus
  = GetCertificateResponse'{certificate = Core.Nothing,
                            certificateChain = Core.Nothing, responseStatus}

-- | The ACM-issued certificate corresponding to the ARN specified as input.
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsCertificate :: Lens.Lens' GetCertificateResponse (Core.Maybe Types.Certificate)
gcrrsCertificate = Lens.field @"certificate"
{-# INLINEABLE gcrrsCertificate #-}
{-# DEPRECATED certificate "Use generic-lens or generic-optics with 'certificate' instead"  #-}

-- | Certificates forming the requested certificate's chain of trust. The chain consists of the certificate of the issuing CA and the intermediate certificates of any other subordinate CAs. 
--
-- /Note:/ Consider using 'certificateChain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsCertificateChain :: Lens.Lens' GetCertificateResponse (Core.Maybe Types.CertificateChain)
gcrrsCertificateChain = Lens.field @"certificateChain"
{-# INLINEABLE gcrrsCertificateChain #-}
{-# DEPRECATED certificateChain "Use generic-lens or generic-optics with 'certificateChain' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcrrsResponseStatus :: Lens.Lens' GetCertificateResponse Core.Int
gcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
