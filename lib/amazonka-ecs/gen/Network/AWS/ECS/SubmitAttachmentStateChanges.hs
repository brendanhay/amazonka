{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.SubmitAttachmentStateChanges
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sent to acknowledge that an attachment changed states.
module Network.AWS.ECS.SubmitAttachmentStateChanges
    (
    -- * Creating a request
      SubmitAttachmentStateChanges (..)
    , mkSubmitAttachmentStateChanges
    -- ** Request lenses
    , sascAttachments
    , sascCluster

    -- * Destructuring the response
    , SubmitAttachmentStateChangesResponse (..)
    , mkSubmitAttachmentStateChangesResponse
    -- ** Response lenses
    , sascrrsAcknowledgment
    , sascrrsResponseStatus
    ) where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSubmitAttachmentStateChanges' smart constructor.
data SubmitAttachmentStateChanges = SubmitAttachmentStateChanges'
  { attachments :: [Types.AttachmentStateChange]
    -- ^ Any attachments associated with the state change request.
  , cluster :: Core.Maybe Core.Text
    -- ^ The short name or full ARN of the cluster that hosts the container instance the attachment belongs to.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SubmitAttachmentStateChanges' value with any optional fields omitted.
mkSubmitAttachmentStateChanges
    :: SubmitAttachmentStateChanges
mkSubmitAttachmentStateChanges
  = SubmitAttachmentStateChanges'{attachments = Core.mempty,
                                  cluster = Core.Nothing}

-- | Any attachments associated with the state change request.
--
-- /Note:/ Consider using 'attachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sascAttachments :: Lens.Lens' SubmitAttachmentStateChanges [Types.AttachmentStateChange]
sascAttachments = Lens.field @"attachments"
{-# INLINEABLE sascAttachments #-}
{-# DEPRECATED attachments "Use generic-lens or generic-optics with 'attachments' instead"  #-}

-- | The short name or full ARN of the cluster that hosts the container instance the attachment belongs to.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sascCluster :: Lens.Lens' SubmitAttachmentStateChanges (Core.Maybe Core.Text)
sascCluster = Lens.field @"cluster"
{-# INLINEABLE sascCluster #-}
{-# DEPRECATED cluster "Use generic-lens or generic-optics with 'cluster' instead"  #-}

instance Core.ToQuery SubmitAttachmentStateChanges where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders SubmitAttachmentStateChanges where
        toHeaders SubmitAttachmentStateChanges{..}
          = Core.pure
              ("X-Amz-Target",
               "AmazonEC2ContainerServiceV20141113.SubmitAttachmentStateChanges")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON SubmitAttachmentStateChanges where
        toJSON SubmitAttachmentStateChanges{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("attachments" Core..= attachments),
                  ("cluster" Core..=) Core.<$> cluster])

instance Core.AWSRequest SubmitAttachmentStateChanges where
        type Rs SubmitAttachmentStateChanges =
             SubmitAttachmentStateChangesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 SubmitAttachmentStateChangesResponse' Core.<$>
                   (x Core..:? "acknowledgment") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSubmitAttachmentStateChangesResponse' smart constructor.
data SubmitAttachmentStateChangesResponse = SubmitAttachmentStateChangesResponse'
  { acknowledgment :: Core.Maybe Core.Text
    -- ^ Acknowledgement of the state change.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SubmitAttachmentStateChangesResponse' value with any optional fields omitted.
mkSubmitAttachmentStateChangesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> SubmitAttachmentStateChangesResponse
mkSubmitAttachmentStateChangesResponse responseStatus
  = SubmitAttachmentStateChangesResponse'{acknowledgment =
                                            Core.Nothing,
                                          responseStatus}

-- | Acknowledgement of the state change.
--
-- /Note:/ Consider using 'acknowledgment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sascrrsAcknowledgment :: Lens.Lens' SubmitAttachmentStateChangesResponse (Core.Maybe Core.Text)
sascrrsAcknowledgment = Lens.field @"acknowledgment"
{-# INLINEABLE sascrrsAcknowledgment #-}
{-# DEPRECATED acknowledgment "Use generic-lens or generic-optics with 'acknowledgment' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sascrrsResponseStatus :: Lens.Lens' SubmitAttachmentStateChangesResponse Core.Int
sascrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE sascrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
