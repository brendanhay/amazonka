{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      RejectAssignment (..)
    , mkRejectAssignment
    -- ** Request lenses
    , raAssignmentId
    , raRequesterFeedback

    -- * Destructuring the response
    , RejectAssignmentResponse (..)
    , mkRejectAssignmentResponse
    -- ** Response lenses
    , rarrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkRejectAssignment' smart constructor.
data RejectAssignment = RejectAssignment'
  { assignmentId :: Types.AssignmentId
    -- ^ The ID of the assignment. The assignment must correspond to a HIT created by the Requester. 
  , requesterFeedback :: Core.Text
    -- ^ A message for the Worker, which the Worker can see in the Status section of the web site. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RejectAssignment' value with any optional fields omitted.
mkRejectAssignment
    :: Types.AssignmentId -- ^ 'assignmentId'
    -> Core.Text -- ^ 'requesterFeedback'
    -> RejectAssignment
mkRejectAssignment assignmentId requesterFeedback
  = RejectAssignment'{assignmentId, requesterFeedback}

-- | The ID of the assignment. The assignment must correspond to a HIT created by the Requester. 
--
-- /Note:/ Consider using 'assignmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raAssignmentId :: Lens.Lens' RejectAssignment Types.AssignmentId
raAssignmentId = Lens.field @"assignmentId"
{-# INLINEABLE raAssignmentId #-}
{-# DEPRECATED assignmentId "Use generic-lens or generic-optics with 'assignmentId' instead"  #-}

-- | A message for the Worker, which the Worker can see in the Status section of the web site. 
--
-- /Note:/ Consider using 'requesterFeedback' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raRequesterFeedback :: Lens.Lens' RejectAssignment Core.Text
raRequesterFeedback = Lens.field @"requesterFeedback"
{-# INLINEABLE raRequesterFeedback #-}
{-# DEPRECATED requesterFeedback "Use generic-lens or generic-optics with 'requesterFeedback' instead"  #-}

instance Core.ToQuery RejectAssignment where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders RejectAssignment where
        toHeaders RejectAssignment{..}
          = Core.pure
              ("X-Amz-Target", "MTurkRequesterServiceV20170117.RejectAssignment")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON RejectAssignment where
        toJSON RejectAssignment{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AssignmentId" Core..= assignmentId),
                  Core.Just ("RequesterFeedback" Core..= requesterFeedback)])

instance Core.AWSRequest RejectAssignment where
        type Rs RejectAssignment = RejectAssignmentResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 RejectAssignmentResponse' Core.<$> (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkRejectAssignmentResponse' smart constructor.
newtype RejectAssignmentResponse = RejectAssignmentResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RejectAssignmentResponse' value with any optional fields omitted.
mkRejectAssignmentResponse
    :: Core.Int -- ^ 'responseStatus'
    -> RejectAssignmentResponse
mkRejectAssignmentResponse responseStatus
  = RejectAssignmentResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rarrsResponseStatus :: Lens.Lens' RejectAssignmentResponse Core.Int
rarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE rarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
