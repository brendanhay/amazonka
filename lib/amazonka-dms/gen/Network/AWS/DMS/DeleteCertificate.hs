{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.DeleteCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified certificate. 
module Network.AWS.DMS.DeleteCertificate
    (
    -- * Creating a request
      DeleteCertificate (..)
    , mkDeleteCertificate
    -- ** Request lenses
    , dcCertificateArn

    -- * Destructuring the response
    , DeleteCertificateResponse (..)
    , mkDeleteCertificateResponse
    -- ** Response lenses
    , dcrgrsCertificate
    , dcrgrsResponseStatus
    ) where

import qualified Network.AWS.DMS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteCertificate' smart constructor.
newtype DeleteCertificate = DeleteCertificate'
  { certificateArn :: Core.Text
    -- ^ The Amazon Resource Name (ARN) of the deleted certificate.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCertificate' value with any optional fields omitted.
mkDeleteCertificate
    :: Core.Text -- ^ 'certificateArn'
    -> DeleteCertificate
mkDeleteCertificate certificateArn
  = DeleteCertificate'{certificateArn}

-- | The Amazon Resource Name (ARN) of the deleted certificate.
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcCertificateArn :: Lens.Lens' DeleteCertificate Core.Text
dcCertificateArn = Lens.field @"certificateArn"
{-# INLINEABLE dcCertificateArn #-}
{-# DEPRECATED certificateArn "Use generic-lens or generic-optics with 'certificateArn' instead"  #-}

instance Core.ToQuery DeleteCertificate where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteCertificate where
        toHeaders DeleteCertificate{..}
          = Core.pure
              ("X-Amz-Target", "AmazonDMSv20160101.DeleteCertificate")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteCertificate where
        toJSON DeleteCertificate{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("CertificateArn" Core..= certificateArn)])

instance Core.AWSRequest DeleteCertificate where
        type Rs DeleteCertificate = DeleteCertificateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteCertificateResponse' Core.<$>
                   (x Core..:? "Certificate") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteCertificateResponse' smart constructor.
data DeleteCertificateResponse = DeleteCertificateResponse'
  { certificate :: Core.Maybe Types.Certificate
    -- ^ The Secure Sockets Layer (SSL) certificate.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeleteCertificateResponse' value with any optional fields omitted.
mkDeleteCertificateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteCertificateResponse
mkDeleteCertificateResponse responseStatus
  = DeleteCertificateResponse'{certificate = Core.Nothing,
                               responseStatus}

-- | The Secure Sockets Layer (SSL) certificate.
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrgrsCertificate :: Lens.Lens' DeleteCertificateResponse (Core.Maybe Types.Certificate)
dcrgrsCertificate = Lens.field @"certificate"
{-# INLINEABLE dcrgrsCertificate #-}
{-# DEPRECATED certificate "Use generic-lens or generic-optics with 'certificate' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrgrsResponseStatus :: Lens.Lens' DeleteCertificateResponse Core.Int
dcrgrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcrgrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
