{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManager.RenewCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Renews an eligable ACM certificate. At this time, only exported private certificates can be renewed with this operation. In order to renew your ACM PCA certificates with ACM, you must first <https://docs.aws.amazon.com/acm-pca/latest/userguide/PcaPermissions.html grant the ACM service principal permission to do so> . For more information, see <https://docs.aws.amazon.com/acm/latest/userguide/manual-renewal.html Testing Managed Renewal> in the ACM User Guide.
module Network.AWS.CertificateManager.RenewCertificate
    (
    -- * Creating a request
      RenewCertificate (..)
    , mkRenewCertificate
    -- ** Request lenses
    , rcCertificateArn

    -- * Destructuring the response
    , RenewCertificateResponse (..)
    , mkRenewCertificateResponse
    ) where

import qualified Network.AWS.CertificateManager.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRenewCertificate' smart constructor.
newtype RenewCertificate = RenewCertificate'
  { certificateArn :: Types.Arn
    -- ^ String that contains the ARN of the ACM certificate to be renewed. This must be of the form:
--
-- @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@ 
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RenewCertificate' value with any optional fields omitted.
mkRenewCertificate
    :: Types.Arn -- ^ 'certificateArn'
    -> RenewCertificate
mkRenewCertificate certificateArn
  = RenewCertificate'{certificateArn}

-- | String that contains the ARN of the ACM certificate to be renewed. This must be of the form:
--
-- @arn:aws:acm:region:123456789012:certificate/12345678-1234-1234-1234-123456789012@ 
-- For more information about ARNs, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcCertificateArn :: Lens.Lens' RenewCertificate Types.Arn
rcCertificateArn = Lens.field @"certificateArn"
{-# INLINEABLE rcCertificateArn #-}
{-# DEPRECATED certificateArn "Use generic-lens or generic-optics with 'certificateArn' instead"  #-}

instance Core.ToQuery RenewCertificate where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RenewCertificate where
        toHeaders RenewCertificate{..}
          = Core.pure ("X-Amz-Target", "CertificateManager.RenewCertificate")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RenewCertificate where
        toJSON RenewCertificate{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("CertificateArn" Core..= certificateArn)])

instance Core.AWSRequest RenewCertificate where
        type Rs RenewCertificate = RenewCertificateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull RenewCertificateResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRenewCertificateResponse' smart constructor.
data RenewCertificateResponse = RenewCertificateResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RenewCertificateResponse' value with any optional fields omitted.
mkRenewCertificateResponse
    :: RenewCertificateResponse
mkRenewCertificateResponse = RenewCertificateResponse'
