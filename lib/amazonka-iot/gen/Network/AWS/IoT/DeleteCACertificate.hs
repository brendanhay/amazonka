{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteCACertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a registered CA certificate.
module Network.AWS.IoT.DeleteCACertificate
    (
    -- * Creating a request
      DeleteCACertificate (..)
    , mkDeleteCACertificate
    -- ** Request lenses
    , dcacCertificateId

    -- * Destructuring the response
    , DeleteCACertificateResponse (..)
    , mkDeleteCACertificateResponse
    -- ** Response lenses
    , dcacrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Input for the DeleteCACertificate operation.
--
-- /See:/ 'mkDeleteCACertificate' smart constructor.
newtype DeleteCACertificate = DeleteCACertificate'
  { certificateId :: Types.CertificateId
    -- ^ The ID of the certificate to delete. (The last part of the certificate ARN contains the certificate ID.)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCACertificate' value with any optional fields omitted.
mkDeleteCACertificate
    :: Types.CertificateId -- ^ 'certificateId'
    -> DeleteCACertificate
mkDeleteCACertificate certificateId
  = DeleteCACertificate'{certificateId}

-- | The ID of the certificate to delete. (The last part of the certificate ARN contains the certificate ID.)
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcacCertificateId :: Lens.Lens' DeleteCACertificate Types.CertificateId
dcacCertificateId = Lens.field @"certificateId"
{-# INLINEABLE dcacCertificateId #-}
{-# DEPRECATED certificateId "Use generic-lens or generic-optics with 'certificateId' instead"  #-}

instance Core.ToQuery DeleteCACertificate where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteCACertificate where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteCACertificate where
        type Rs DeleteCACertificate = DeleteCACertificateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/cacertificate/" Core.<> Core.toText certificateId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 DeleteCACertificateResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | The output for the DeleteCACertificate operation.
--
-- /See:/ 'mkDeleteCACertificateResponse' smart constructor.
newtype DeleteCACertificateResponse = DeleteCACertificateResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCACertificateResponse' value with any optional fields omitted.
mkDeleteCACertificateResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteCACertificateResponse
mkDeleteCACertificateResponse responseStatus
  = DeleteCACertificateResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcacrrsResponseStatus :: Lens.Lens' DeleteCACertificateResponse Core.Int
dcacrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcacrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
