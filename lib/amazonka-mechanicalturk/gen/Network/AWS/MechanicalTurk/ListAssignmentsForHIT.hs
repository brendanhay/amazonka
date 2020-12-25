{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.ListAssignmentsForHIT
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ListAssignmentsForHIT@ operation retrieves completed assignments for a HIT. You can use this operation to retrieve the results for a HIT.
--
-- You can get assignments for a HIT at any time, even if the HIT is not yet Reviewable. If a HIT requested multiple assignments, and has received some results but has not yet become Reviewable, you can still retrieve the partial results with this operation.
-- Use the AssignmentStatus parameter to control which set of assignments for a HIT are returned. The ListAssignmentsForHIT operation can return submitted assignments awaiting approval, or it can return assignments that have already been approved or rejected. You can set AssignmentStatus=Approved,Rejected to get assignments that have already been approved and rejected together in one result set.
-- Only the Requester who created the HIT can retrieve the assignments for that HIT.
-- Results are sorted and divided into numbered pages and the operation returns a single page of results. You can use the parameters of the operation to control sorting and pagination.
--
-- This operation returns paginated results.
module Network.AWS.MechanicalTurk.ListAssignmentsForHIT
  ( -- * Creating a request
    ListAssignmentsForHIT (..),
    mkListAssignmentsForHIT,

    -- ** Request lenses
    lafhitHITId,
    lafhitAssignmentStatuses,
    lafhitMaxResults,
    lafhitNextToken,

    -- * Destructuring the response
    ListAssignmentsForHITResponse (..),
    mkListAssignmentsForHITResponse,

    -- ** Response lenses
    lafhitrrsAssignments,
    lafhitrrsNextToken,
    lafhitrrsNumResults,
    lafhitrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListAssignmentsForHIT' smart constructor.
data ListAssignmentsForHIT = ListAssignmentsForHIT'
  { -- | The ID of the HIT.
    hITId :: Types.HITId,
    -- | The status of the assignments to return: Submitted | Approved | Rejected
    assignmentStatuses :: Core.Maybe [Types.AssignmentStatus],
    maxResults :: Core.Maybe Core.Natural,
    -- | Pagination token
    nextToken :: Core.Maybe Types.PaginationToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListAssignmentsForHIT' value with any optional fields omitted.
mkListAssignmentsForHIT ::
  -- | 'hITId'
  Types.HITId ->
  ListAssignmentsForHIT
mkListAssignmentsForHIT hITId =
  ListAssignmentsForHIT'
    { hITId,
      assignmentStatuses = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The ID of the HIT.
--
-- /Note:/ Consider using 'hITId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafhitHITId :: Lens.Lens' ListAssignmentsForHIT Types.HITId
lafhitHITId = Lens.field @"hITId"
{-# DEPRECATED lafhitHITId "Use generic-lens or generic-optics with 'hITId' instead." #-}

-- | The status of the assignments to return: Submitted | Approved | Rejected
--
-- /Note:/ Consider using 'assignmentStatuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafhitAssignmentStatuses :: Lens.Lens' ListAssignmentsForHIT (Core.Maybe [Types.AssignmentStatus])
lafhitAssignmentStatuses = Lens.field @"assignmentStatuses"
{-# DEPRECATED lafhitAssignmentStatuses "Use generic-lens or generic-optics with 'assignmentStatuses' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafhitMaxResults :: Lens.Lens' ListAssignmentsForHIT (Core.Maybe Core.Natural)
lafhitMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lafhitMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Pagination token
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafhitNextToken :: Lens.Lens' ListAssignmentsForHIT (Core.Maybe Types.PaginationToken)
lafhitNextToken = Lens.field @"nextToken"
{-# DEPRECATED lafhitNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListAssignmentsForHIT where
  toJSON ListAssignmentsForHIT {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("HITId" Core..= hITId),
            ("AssignmentStatuses" Core..=) Core.<$> assignmentStatuses,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListAssignmentsForHIT where
  type Rs ListAssignmentsForHIT = ListAssignmentsForHITResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "MTurkRequesterServiceV20170117.ListAssignmentsForHIT"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAssignmentsForHITResponse'
            Core.<$> (x Core..:? "Assignments")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "NumResults")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListAssignmentsForHIT where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"assignments" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListAssignmentsForHITResponse' smart constructor.
data ListAssignmentsForHITResponse = ListAssignmentsForHITResponse'
  { -- | The collection of Assignment data structures returned by this call.
    assignments :: Core.Maybe [Types.Assignment],
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | The number of assignments on the page in the filtered results list, equivalent to the number of assignments returned by this call.
    numResults :: Core.Maybe Core.Int,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListAssignmentsForHITResponse' value with any optional fields omitted.
mkListAssignmentsForHITResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListAssignmentsForHITResponse
mkListAssignmentsForHITResponse responseStatus =
  ListAssignmentsForHITResponse'
    { assignments = Core.Nothing,
      nextToken = Core.Nothing,
      numResults = Core.Nothing,
      responseStatus
    }

-- | The collection of Assignment data structures returned by this call.
--
-- /Note:/ Consider using 'assignments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafhitrrsAssignments :: Lens.Lens' ListAssignmentsForHITResponse (Core.Maybe [Types.Assignment])
lafhitrrsAssignments = Lens.field @"assignments"
{-# DEPRECATED lafhitrrsAssignments "Use generic-lens or generic-optics with 'assignments' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafhitrrsNextToken :: Lens.Lens' ListAssignmentsForHITResponse (Core.Maybe Types.PaginationToken)
lafhitrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lafhitrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The number of assignments on the page in the filtered results list, equivalent to the number of assignments returned by this call.
--
-- /Note:/ Consider using 'numResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafhitrrsNumResults :: Lens.Lens' ListAssignmentsForHITResponse (Core.Maybe Core.Int)
lafhitrrsNumResults = Lens.field @"numResults"
{-# DEPRECATED lafhitrrsNumResults "Use generic-lens or generic-optics with 'numResults' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lafhitrrsResponseStatus :: Lens.Lens' ListAssignmentsForHITResponse Core.Int
lafhitrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lafhitrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
