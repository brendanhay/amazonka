{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.GetAssignment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @GetAssignment@ operation retrieves the details of the specified Assignment. 
module Network.AWS.MechanicalTurk.GetAssignment
    (
    -- * Creating a request
      GetAssignment (..)
    , mkGetAssignment
    -- ** Request lenses
    , gaAssignmentId

    -- * Destructuring the response
    , GetAssignmentResponse (..)
    , mkGetAssignmentResponse
    -- ** Response lenses
    , garrsAssignment
    , garrsHIT
    , garrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetAssignment' smart constructor.
newtype GetAssignment = GetAssignment'
  { assignmentId :: Types.AssignmentId
    -- ^ The ID of the Assignment to be retrieved.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'GetAssignment' value with any optional fields omitted.
mkGetAssignment
    :: Types.AssignmentId -- ^ 'assignmentId'
    -> GetAssignment
mkGetAssignment assignmentId = GetAssignment'{assignmentId}

-- | The ID of the Assignment to be retrieved.
--
-- /Note:/ Consider using 'assignmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaAssignmentId :: Lens.Lens' GetAssignment Types.AssignmentId
gaAssignmentId = Lens.field @"assignmentId"
{-# INLINEABLE gaAssignmentId #-}
{-# DEPRECATED assignmentId "Use generic-lens or generic-optics with 'assignmentId' instead"  #-}

instance Core.ToQuery GetAssignment where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetAssignment where
        toHeaders GetAssignment{..}
          = Core.pure
              ("X-Amz-Target", "MTurkRequesterServiceV20170117.GetAssignment")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetAssignment where
        toJSON GetAssignment{..}
          = Core.object
              (Core.catMaybes [Core.Just ("AssignmentId" Core..= assignmentId)])

instance Core.AWSRequest GetAssignment where
        type Rs GetAssignment = GetAssignmentResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetAssignmentResponse' Core.<$>
                   (x Core..:? "Assignment") Core.<*> x Core..:? "HIT" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetAssignmentResponse' smart constructor.
data GetAssignmentResponse = GetAssignmentResponse'
  { assignment :: Core.Maybe Types.Assignment
    -- ^ The assignment. The response includes one Assignment element. 
  , hit :: Core.Maybe Types.HIT
    -- ^ The HIT associated with this assignment. The response includes one HIT element.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetAssignmentResponse' value with any optional fields omitted.
mkGetAssignmentResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetAssignmentResponse
mkGetAssignmentResponse responseStatus
  = GetAssignmentResponse'{assignment = Core.Nothing,
                           hit = Core.Nothing, responseStatus}

-- | The assignment. The response includes one Assignment element. 
--
-- /Note:/ Consider using 'assignment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsAssignment :: Lens.Lens' GetAssignmentResponse (Core.Maybe Types.Assignment)
garrsAssignment = Lens.field @"assignment"
{-# INLINEABLE garrsAssignment #-}
{-# DEPRECATED assignment "Use generic-lens or generic-optics with 'assignment' instead"  #-}

-- | The HIT associated with this assignment. The response includes one HIT element.
--
-- /Note:/ Consider using 'hit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsHIT :: Lens.Lens' GetAssignmentResponse (Core.Maybe Types.HIT)
garrsHIT = Lens.field @"hit"
{-# INLINEABLE garrsHIT #-}
{-# DEPRECATED hit "Use generic-lens or generic-optics with 'hit' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsResponseStatus :: Lens.Lens' GetAssignmentResponse Core.Int
garrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE garrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
