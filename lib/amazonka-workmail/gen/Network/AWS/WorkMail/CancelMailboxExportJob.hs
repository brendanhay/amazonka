{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.CancelMailboxExportJob
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels a mailbox export job.
module Network.AWS.WorkMail.CancelMailboxExportJob
    (
    -- * Creating a request
      CancelMailboxExportJob (..)
    , mkCancelMailboxExportJob
    -- ** Request lenses
    , cmejClientToken
    , cmejJobId
    , cmejOrganizationId

    -- * Destructuring the response
    , CancelMailboxExportJobResponse (..)
    , mkCancelMailboxExportJobResponse
    -- ** Response lenses
    , cmejrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkMail.Types as Types

-- | /See:/ 'mkCancelMailboxExportJob' smart constructor.
data CancelMailboxExportJob = CancelMailboxExportJob'
  { clientToken :: Types.IdempotencyClientToken
    -- ^ The idempotency token for the client request.
  , jobId :: Types.MailboxExportJobId
    -- ^ The job ID.
  , organizationId :: Types.OrganizationId
    -- ^ The organization ID.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CancelMailboxExportJob' value with any optional fields omitted.
mkCancelMailboxExportJob
    :: Types.IdempotencyClientToken -- ^ 'clientToken'
    -> Types.MailboxExportJobId -- ^ 'jobId'
    -> Types.OrganizationId -- ^ 'organizationId'
    -> CancelMailboxExportJob
mkCancelMailboxExportJob clientToken jobId organizationId
  = CancelMailboxExportJob'{clientToken, jobId, organizationId}

-- | The idempotency token for the client request.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmejClientToken :: Lens.Lens' CancelMailboxExportJob Types.IdempotencyClientToken
cmejClientToken = Lens.field @"clientToken"
{-# INLINEABLE cmejClientToken #-}
{-# DEPRECATED clientToken "Use generic-lens or generic-optics with 'clientToken' instead"  #-}

-- | The job ID.
--
-- /Note:/ Consider using 'jobId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmejJobId :: Lens.Lens' CancelMailboxExportJob Types.MailboxExportJobId
cmejJobId = Lens.field @"jobId"
{-# INLINEABLE cmejJobId #-}
{-# DEPRECATED jobId "Use generic-lens or generic-optics with 'jobId' instead"  #-}

-- | The organization ID.
--
-- /Note:/ Consider using 'organizationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmejOrganizationId :: Lens.Lens' CancelMailboxExportJob Types.OrganizationId
cmejOrganizationId = Lens.field @"organizationId"
{-# INLINEABLE cmejOrganizationId #-}
{-# DEPRECATED organizationId "Use generic-lens or generic-optics with 'organizationId' instead"  #-}

instance Core.ToQuery CancelMailboxExportJob where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CancelMailboxExportJob where
        toHeaders CancelMailboxExportJob{..}
          = Core.pure
              ("X-Amz-Target", "WorkMailService.CancelMailboxExportJob")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CancelMailboxExportJob where
        toJSON CancelMailboxExportJob{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ClientToken" Core..= clientToken),
                  Core.Just ("JobId" Core..= jobId),
                  Core.Just ("OrganizationId" Core..= organizationId)])

instance Core.AWSRequest CancelMailboxExportJob where
        type Rs CancelMailboxExportJob = CancelMailboxExportJobResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 CancelMailboxExportJobResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCancelMailboxExportJobResponse' smart constructor.
newtype CancelMailboxExportJobResponse = CancelMailboxExportJobResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CancelMailboxExportJobResponse' value with any optional fields omitted.
mkCancelMailboxExportJobResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CancelMailboxExportJobResponse
mkCancelMailboxExportJobResponse responseStatus
  = CancelMailboxExportJobResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmejrrsResponseStatus :: Lens.Lens' CancelMailboxExportJobResponse Core.Int
cmejrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cmejrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
