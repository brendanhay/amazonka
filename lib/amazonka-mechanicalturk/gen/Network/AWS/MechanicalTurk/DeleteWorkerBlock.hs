{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.DeleteWorkerBlock
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @DeleteWorkerBlock@ operation allows you to reinstate a blocked Worker to work on your HITs. This operation reverses the effects of the CreateWorkerBlock operation. You need the Worker ID to use this operation. If the Worker ID is missing or invalid, this operation fails and returns the message “WorkerId is invalid.” If the specified Worker is not blocked, this operation returns successfully.
module Network.AWS.MechanicalTurk.DeleteWorkerBlock
  ( -- * Creating a request
    DeleteWorkerBlock (..),
    mkDeleteWorkerBlock,

    -- ** Request lenses
    dwbWorkerId,
    dwbReason,

    -- * Destructuring the response
    DeleteWorkerBlockResponse (..),
    mkDeleteWorkerBlockResponse,

    -- ** Response lenses
    dwbrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDeleteWorkerBlock' smart constructor.
data DeleteWorkerBlock = DeleteWorkerBlock'
  { -- | The ID of the Worker to unblock.
    workerId :: Types.CustomerId,
    -- | A message that explains the reason for unblocking the Worker. The Worker does not see this message.
    reason :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteWorkerBlock' value with any optional fields omitted.
mkDeleteWorkerBlock ::
  -- | 'workerId'
  Types.CustomerId ->
  DeleteWorkerBlock
mkDeleteWorkerBlock workerId =
  DeleteWorkerBlock' {workerId, reason = Core.Nothing}

-- | The ID of the Worker to unblock.
--
-- /Note:/ Consider using 'workerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwbWorkerId :: Lens.Lens' DeleteWorkerBlock Types.CustomerId
dwbWorkerId = Lens.field @"workerId"
{-# DEPRECATED dwbWorkerId "Use generic-lens or generic-optics with 'workerId' instead." #-}

-- | A message that explains the reason for unblocking the Worker. The Worker does not see this message.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwbReason :: Lens.Lens' DeleteWorkerBlock (Core.Maybe Types.String)
dwbReason = Lens.field @"reason"
{-# DEPRECATED dwbReason "Use generic-lens or generic-optics with 'reason' instead." #-}

instance Core.FromJSON DeleteWorkerBlock where
  toJSON DeleteWorkerBlock {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("WorkerId" Core..= workerId),
            ("Reason" Core..=) Core.<$> reason
          ]
      )

instance Core.AWSRequest DeleteWorkerBlock where
  type Rs DeleteWorkerBlock = DeleteWorkerBlockResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "MTurkRequesterServiceV20170117.DeleteWorkerBlock"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteWorkerBlockResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkDeleteWorkerBlockResponse' smart constructor.
newtype DeleteWorkerBlockResponse = DeleteWorkerBlockResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteWorkerBlockResponse' value with any optional fields omitted.
mkDeleteWorkerBlockResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DeleteWorkerBlockResponse
mkDeleteWorkerBlockResponse responseStatus =
  DeleteWorkerBlockResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwbrrsResponseStatus :: Lens.Lens' DeleteWorkerBlockResponse Core.Int
dwbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dwbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
