{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DeleteCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified certificate.
--
-- A certificate cannot be deleted if it has a policy or IoT thing attached to it or if its status is set to ACTIVE. To delete a certificate, first use the 'DetachPrincipalPolicy' API to detach all policies. Next, use the 'UpdateCertificate' API to set the certificate to the INACTIVE status.
module Network.AWS.IoT.DeleteCertificate
    (
    -- * Creating a request
      DeleteCertificate (..)
    , mkDeleteCertificate
    -- ** Request lenses
    , dcCertificateId
    , dcForceDelete

    -- * Destructuring the response
    , DeleteCertificateResponse (..)
    , mkDeleteCertificateResponse
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the DeleteCertificate operation.
--
-- /See:/ 'mkDeleteCertificate' smart constructor.
data DeleteCertificate = DeleteCertificate'
  { certificateId :: Types.CertificateId
    -- ^ The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
  , forceDelete :: Core.Maybe Core.Bool
    -- ^ Forces the deletion of a certificate if it is inactive and is not attached to an IoT thing.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCertificate' value with any optional fields omitted.
mkDeleteCertificate
    :: Types.CertificateId -- ^ 'certificateId'
    -> DeleteCertificate
mkDeleteCertificate certificateId
  = DeleteCertificate'{certificateId, forceDelete = Core.Nothing}

-- | The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcCertificateId :: Lens.Lens' DeleteCertificate Types.CertificateId
dcCertificateId = Lens.field @"certificateId"
{-# INLINEABLE dcCertificateId #-}
{-# DEPRECATED certificateId "Use generic-lens or generic-optics with 'certificateId' instead"  #-}

-- | Forces the deletion of a certificate if it is inactive and is not attached to an IoT thing.
--
-- /Note:/ Consider using 'forceDelete' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcForceDelete :: Lens.Lens' DeleteCertificate (Core.Maybe Core.Bool)
dcForceDelete = Lens.field @"forceDelete"
{-# INLINEABLE dcForceDelete #-}
{-# DEPRECATED forceDelete "Use generic-lens or generic-optics with 'forceDelete' instead"  #-}

instance Core.ToQuery DeleteCertificate where
        toQuery DeleteCertificate{..}
          = Core.maybe Core.mempty (Core.toQueryPair "forceDelete")
              forceDelete

instance Core.ToHeaders DeleteCertificate where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DeleteCertificate where
        type Rs DeleteCertificate = DeleteCertificateResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath = "/certificates/" Core.<> Core.toText certificateId,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse = Response.receiveNull DeleteCertificateResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDeleteCertificateResponse' smart constructor.
data DeleteCertificateResponse = DeleteCertificateResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteCertificateResponse' value with any optional fields omitted.
mkDeleteCertificateResponse
    :: DeleteCertificateResponse
mkDeleteCertificateResponse = DeleteCertificateResponse'
