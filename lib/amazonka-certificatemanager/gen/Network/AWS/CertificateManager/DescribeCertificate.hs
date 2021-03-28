{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.DescribeCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns detailed metadata about the specified ACM certificate.
module Network.AWS.CertificateManager.DescribeCertificate
    (
    -- * Creating a request
      DescribeCertificate (..)
    , mkDescribeCertificate
    -- ** Request lenses
    , dCertificateArn

    -- * Destructuring the response
    , DescribeCertificateResponse (..)
    , mkDescribeCertificateResponse
    -- ** Response lenses
    , dcrrsCertificate
    , dcrrsResponseStatus
    ) where

import qualified Network.AWS.CertificateManager.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeCertificate' smart constructor.
newtype DescribeCertificate = DescribeCertificate'
  { certificateArn :: Types.Arn
    -- ^ The Amazon Resource Name (ARN) of the ACM certificate. The ARN must have the following form:
--
-- @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@ 
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCertificate' value with any optional fields omitted.
mkDescribeCertificate
    :: Types.Arn -- ^ 'certificateArn'
    -> DescribeCertificate
mkDescribeCertificate certificateArn
  = DescribeCertificate'{certificateArn}

-- | The Amazon Resource Name (ARN) of the ACM certificate. The ARN must have the following form:
--
-- @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@ 
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dCertificateArn :: Lens.Lens' DescribeCertificate Types.Arn
dCertificateArn = Lens.field @"certificateArn"
{-# INLINEABLE dCertificateArn #-}
{-# DEPRECATED certificateArn "Use generic-lens or generic-optics with 'certificateArn' instead"  #-}

instance Core.ToQuery DescribeCertificate where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeCertificate where
        toHeaders DescribeCertificate{..}
          = Core.pure
              ("X-Amz-Target", "CertificateManager.DescribeCertificate")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeCertificate where
        toJSON DescribeCertificate{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("CertificateArn" Core..= certificateArn)])

instance Core.AWSRequest DescribeCertificate where
        type Rs DescribeCertificate = DescribeCertificateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeCertificateResponse' Core.<$>
                   (x Core..:? "Certificate") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeCertificateResponse' smart constructor.
data DescribeCertificateResponse = DescribeCertificateResponse'
  { certificate :: Core.Maybe Types.CertificateDetail
    -- ^ Metadata about an ACM certificate.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeCertificateResponse' value with any optional fields omitted.
mkDescribeCertificateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeCertificateResponse
mkDescribeCertificateResponse responseStatus
  = DescribeCertificateResponse'{certificate = Core.Nothing,
                                 responseStatus}

-- | Metadata about an ACM certificate.
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsCertificate :: Lens.Lens' DescribeCertificateResponse (Core.Maybe Types.CertificateDetail)
dcrrsCertificate = Lens.field @"certificate"
{-# INLINEABLE dcrrsCertificate #-}
{-# DEPRECATED certificate "Use generic-lens or generic-optics with 'certificate' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsResponseStatus :: Lens.Lens' DescribeCertificateResponse Core.Int
dcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
