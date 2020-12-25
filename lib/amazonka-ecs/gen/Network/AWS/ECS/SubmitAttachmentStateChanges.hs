{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    SubmitAttachmentStateChanges (..),
    mkSubmitAttachmentStateChanges,

    -- ** Request lenses
    sascAttachments,
    sascCluster,

    -- * Destructuring the response
    SubmitAttachmentStateChangesResponse (..),
    mkSubmitAttachmentStateChangesResponse,

    -- ** Response lenses
    sascrrsAcknowledgment,
    sascrrsResponseStatus,
  )
where

import qualified Network.AWS.ECS.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSubmitAttachmentStateChanges' smart constructor.
data SubmitAttachmentStateChanges = SubmitAttachmentStateChanges'
  { -- | Any attachments associated with the state change request.
    attachments :: [Types.AttachmentStateChange],
    -- | The short name or full ARN of the cluster that hosts the container instance the attachment belongs to.
    cluster :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SubmitAttachmentStateChanges' value with any optional fields omitted.
mkSubmitAttachmentStateChanges ::
  SubmitAttachmentStateChanges
mkSubmitAttachmentStateChanges =
  SubmitAttachmentStateChanges'
    { attachments = Core.mempty,
      cluster = Core.Nothing
    }

-- | Any attachments associated with the state change request.
--
-- /Note:/ Consider using 'attachments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sascAttachments :: Lens.Lens' SubmitAttachmentStateChanges [Types.AttachmentStateChange]
sascAttachments = Lens.field @"attachments"
{-# DEPRECATED sascAttachments "Use generic-lens or generic-optics with 'attachments' instead." #-}

-- | The short name or full ARN of the cluster that hosts the container instance the attachment belongs to.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sascCluster :: Lens.Lens' SubmitAttachmentStateChanges (Core.Maybe Types.String)
sascCluster = Lens.field @"cluster"
{-# DEPRECATED sascCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

instance Core.FromJSON SubmitAttachmentStateChanges where
  toJSON SubmitAttachmentStateChanges {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("attachments" Core..= attachments),
            ("cluster" Core..=) Core.<$> cluster
          ]
      )

instance Core.AWSRequest SubmitAttachmentStateChanges where
  type
    Rs SubmitAttachmentStateChanges =
      SubmitAttachmentStateChangesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AmazonEC2ContainerServiceV20141113.SubmitAttachmentStateChanges"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          SubmitAttachmentStateChangesResponse'
            Core.<$> (x Core..:? "acknowledgment")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkSubmitAttachmentStateChangesResponse' smart constructor.
data SubmitAttachmentStateChangesResponse = SubmitAttachmentStateChangesResponse'
  { -- | Acknowledgement of the state change.
    acknowledgment :: Core.Maybe Types.Acknowledgment,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SubmitAttachmentStateChangesResponse' value with any optional fields omitted.
mkSubmitAttachmentStateChangesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  SubmitAttachmentStateChangesResponse
mkSubmitAttachmentStateChangesResponse responseStatus =
  SubmitAttachmentStateChangesResponse'
    { acknowledgment =
        Core.Nothing,
      responseStatus
    }

-- | Acknowledgement of the state change.
--
-- /Note:/ Consider using 'acknowledgment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sascrrsAcknowledgment :: Lens.Lens' SubmitAttachmentStateChangesResponse (Core.Maybe Types.Acknowledgment)
sascrrsAcknowledgment = Lens.field @"acknowledgment"
{-# DEPRECATED sascrrsAcknowledgment "Use generic-lens or generic-optics with 'acknowledgment' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sascrrsResponseStatus :: Lens.Lens' SubmitAttachmentStateChangesResponse Core.Int
sascrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED sascrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
