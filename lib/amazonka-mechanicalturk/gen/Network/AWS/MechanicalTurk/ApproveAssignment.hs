{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.ApproveAssignment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ApproveAssignment@ operation approves the results of a completed assignment.
--
-- Approving an assignment initiates two payments from the Requester's Amazon.com account
--
--     * The Worker who submitted the results is paid the reward specified in the HIT.
--
--
--     * Amazon Mechanical Turk fees are debited.
--
--
-- If the Requester's account does not have adequate funds for these payments, the call to ApproveAssignment returns an exception, and the approval is not processed. You can include an optional feedback message with the approval, which the Worker can see in the Status section of the web site.
-- You can also call this operation for assignments that were previous rejected and approve them by explicitly overriding the previous rejection. This only works on rejected assignments that were submitted within the previous 30 days and only if the assignment's related HIT has not been deleted.
module Network.AWS.MechanicalTurk.ApproveAssignment
  ( -- * Creating a request
    ApproveAssignment (..),
    mkApproveAssignment,

    -- ** Request lenses
    aaAssignmentId,
    aaOverrideRejection,
    aaRequesterFeedback,

    -- * Destructuring the response
    ApproveAssignmentResponse (..),
    mkApproveAssignmentResponse,

    -- ** Response lenses
    aarrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkApproveAssignment' smart constructor.
data ApproveAssignment = ApproveAssignment'
  { -- | The ID of the assignment. The assignment must correspond to a HIT created by the Requester.
    assignmentId :: Types.AssignmentId,
    -- | A flag indicating that an assignment should be approved even if it was previously rejected. Defaults to @False@ .
    overrideRejection :: Core.Maybe Core.Bool,
    -- | A message for the Worker, which the Worker can see in the Status section of the web site.
    requesterFeedback :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ApproveAssignment' value with any optional fields omitted.
mkApproveAssignment ::
  -- | 'assignmentId'
  Types.AssignmentId ->
  ApproveAssignment
mkApproveAssignment assignmentId =
  ApproveAssignment'
    { assignmentId,
      overrideRejection = Core.Nothing,
      requesterFeedback = Core.Nothing
    }

-- | The ID of the assignment. The assignment must correspond to a HIT created by the Requester.
--
-- /Note:/ Consider using 'assignmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaAssignmentId :: Lens.Lens' ApproveAssignment Types.AssignmentId
aaAssignmentId = Lens.field @"assignmentId"
{-# DEPRECATED aaAssignmentId "Use generic-lens or generic-optics with 'assignmentId' instead." #-}

-- | A flag indicating that an assignment should be approved even if it was previously rejected. Defaults to @False@ .
--
-- /Note:/ Consider using 'overrideRejection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaOverrideRejection :: Lens.Lens' ApproveAssignment (Core.Maybe Core.Bool)
aaOverrideRejection = Lens.field @"overrideRejection"
{-# DEPRECATED aaOverrideRejection "Use generic-lens or generic-optics with 'overrideRejection' instead." #-}

-- | A message for the Worker, which the Worker can see in the Status section of the web site.
--
-- /Note:/ Consider using 'requesterFeedback' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaRequesterFeedback :: Lens.Lens' ApproveAssignment (Core.Maybe Types.String)
aaRequesterFeedback = Lens.field @"requesterFeedback"
{-# DEPRECATED aaRequesterFeedback "Use generic-lens or generic-optics with 'requesterFeedback' instead." #-}

instance Core.FromJSON ApproveAssignment where
  toJSON ApproveAssignment {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AssignmentId" Core..= assignmentId),
            ("OverrideRejection" Core..=) Core.<$> overrideRejection,
            ("RequesterFeedback" Core..=) Core.<$> requesterFeedback
          ]
      )

instance Core.AWSRequest ApproveAssignment where
  type Rs ApproveAssignment = ApproveAssignmentResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "MTurkRequesterServiceV20170117.ApproveAssignment"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          ApproveAssignmentResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkApproveAssignmentResponse' smart constructor.
newtype ApproveAssignmentResponse = ApproveAssignmentResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ApproveAssignmentResponse' value with any optional fields omitted.
mkApproveAssignmentResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ApproveAssignmentResponse
mkApproveAssignmentResponse responseStatus =
  ApproveAssignmentResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aarrsResponseStatus :: Lens.Lens' ApproveAssignmentResponse Core.Int
aarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED aarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
