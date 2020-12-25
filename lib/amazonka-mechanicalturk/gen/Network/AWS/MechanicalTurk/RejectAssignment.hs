{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.RejectAssignment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @RejectAssignment@ operation rejects the results of a completed assignment.
--
-- You can include an optional feedback message with the rejection, which the Worker can see in the Status section of the web site. When you include a feedback message with the rejection, it helps the Worker understand why the assignment was rejected, and can improve the quality of the results the Worker submits in the future.
-- Only the Requester who created the HIT can reject an assignment for the HIT.
module Network.AWS.MechanicalTurk.RejectAssignment
  ( -- * Creating a request
    RejectAssignment (..),
    mkRejectAssignment,

    -- ** Request lenses
    raAssignmentId,
    raRequesterFeedback,

    -- * Destructuring the response
    RejectAssignmentResponse (..),
    mkRejectAssignmentResponse,

    -- ** Response lenses
    rarrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRejectAssignment' smart constructor.
data RejectAssignment = RejectAssignment'
  { -- | The ID of the assignment. The assignment must correspond to a HIT created by the Requester.
    assignmentId :: Types.AssignmentId,
    -- | A message for the Worker, which the Worker can see in the Status section of the web site.
    requesterFeedback :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RejectAssignment' value with any optional fields omitted.
mkRejectAssignment ::
  -- | 'assignmentId'
  Types.AssignmentId ->
  -- | 'requesterFeedback'
  Types.String ->
  RejectAssignment
mkRejectAssignment assignmentId requesterFeedback =
  RejectAssignment' {assignmentId, requesterFeedback}

-- | The ID of the assignment. The assignment must correspond to a HIT created by the Requester.
--
-- /Note:/ Consider using 'assignmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raAssignmentId :: Lens.Lens' RejectAssignment Types.AssignmentId
raAssignmentId = Lens.field @"assignmentId"
{-# DEPRECATED raAssignmentId "Use generic-lens or generic-optics with 'assignmentId' instead." #-}

-- | A message for the Worker, which the Worker can see in the Status section of the web site.
--
-- /Note:/ Consider using 'requesterFeedback' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raRequesterFeedback :: Lens.Lens' RejectAssignment Types.String
raRequesterFeedback = Lens.field @"requesterFeedback"
{-# DEPRECATED raRequesterFeedback "Use generic-lens or generic-optics with 'requesterFeedback' instead." #-}

instance Core.FromJSON RejectAssignment where
  toJSON RejectAssignment {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AssignmentId" Core..= assignmentId),
            Core.Just ("RequesterFeedback" Core..= requesterFeedback)
          ]
      )

instance Core.AWSRequest RejectAssignment where
  type Rs RejectAssignment = RejectAssignmentResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "MTurkRequesterServiceV20170117.RejectAssignment")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          RejectAssignmentResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRejectAssignmentResponse' smart constructor.
newtype RejectAssignmentResponse = RejectAssignmentResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RejectAssignmentResponse' value with any optional fields omitted.
mkRejectAssignmentResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RejectAssignmentResponse
mkRejectAssignmentResponse responseStatus =
  RejectAssignmentResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rarrsResponseStatus :: Lens.Lens' RejectAssignmentResponse Core.Int
rarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
