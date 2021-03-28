{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an SSL/TLS certificate for your Amazon Lightsail content delivery network (CDN) distribution.
--
-- Certificates that are currently attached to a distribution cannot be deleted. Use the @DetachCertificateFromDistribution@ action to detach a certificate from a distribution.
module Network.AWS.Lightsail.DeleteCertificate
    (
    -- * Creating a request
      DeleteCertificate (..)
    , mkDeleteCertificate
    -- ** Request lenses
    , dcCertificateName

    -- * Destructuring the response
    , DeleteCertificateResponse (..)
    , mkDeleteCertificateResponse
    -- ** Response lenses
    , dcrrsOperations
    , dcrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteCertificate' smart constructor.
newtype DeleteCertificate = DeleteCertificate'
  { certificateName :: Types.CertificateName
    -- ^ The name of the certificate to delete.
--
-- Use the @GetCertificates@ action to get a list of certificate names that you can specify.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCertificate' value with any optional fields omitted.
mkDeleteCertificate
    :: Types.CertificateName -- ^ 'certificateName'
    -> DeleteCertificate
mkDeleteCertificate certificateName
  = DeleteCertificate'{certificateName}

-- | The name of the certificate to delete.
--
-- Use the @GetCertificates@ action to get a list of certificate names that you can specify.
--
-- /Note:/ Consider using 'certificateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcCertificateName :: Lens.Lens' DeleteCertificate Types.CertificateName
dcCertificateName = Lens.field @"certificateName"
{-# INLINEABLE dcCertificateName #-}
{-# DEPRECATED certificateName "Use generic-lens or generic-optics with 'certificateName' instead"  #-}

instance Core.ToQuery DeleteCertificate where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteCertificate where
        toHeaders DeleteCertificate{..}
          = Core.pure
              ("X-Amz-Target", "Lightsail_20161128.DeleteCertificate")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DeleteCertificate where
        toJSON DeleteCertificate{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("certificateName" Core..= certificateName)])

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
                   (x Core..:? "operations") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteCertificateResponse' smart constructor.
data DeleteCertificateResponse = DeleteCertificateResponse'
  { operations :: Core.Maybe [Types.Operation]
    -- ^ An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
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
  = DeleteCertificateResponse'{operations = Core.Nothing,
                               responseStatus}

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsOperations :: Lens.Lens' DeleteCertificateResponse (Core.Maybe [Types.Operation])
dcrrsOperations = Lens.field @"operations"
{-# INLINEABLE dcrrsOperations #-}
{-# DEPRECATED operations "Use generic-lens or generic-optics with 'operations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrrsResponseStatus :: Lens.Lens' DeleteCertificateResponse Core.Int
dcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
