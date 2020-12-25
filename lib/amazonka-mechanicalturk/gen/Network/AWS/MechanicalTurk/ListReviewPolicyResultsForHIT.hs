{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.ListReviewPolicyResultsForHIT
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @ListReviewPolicyResultsForHIT@ operation retrieves the computed results and the actions taken in the course of executing your Review Policies for a given HIT. For information about how to specify Review Policies when you call CreateHIT, see Review Policies. The ListReviewPolicyResultsForHIT operation can return results for both Assignment-level and HIT-level review results.
module Network.AWS.MechanicalTurk.ListReviewPolicyResultsForHIT
  ( -- * Creating a request
    ListReviewPolicyResultsForHIT (..),
    mkListReviewPolicyResultsForHIT,

    -- ** Request lenses
    lrprfhitHITId,
    lrprfhitMaxResults,
    lrprfhitNextToken,
    lrprfhitPolicyLevels,
    lrprfhitRetrieveActions,
    lrprfhitRetrieveResults,

    -- * Destructuring the response
    ListReviewPolicyResultsForHITResponse (..),
    mkListReviewPolicyResultsForHITResponse,

    -- ** Response lenses
    lrprfhitrrsAssignmentReviewPolicy,
    lrprfhitrrsAssignmentReviewReport,
    lrprfhitrrsHITId,
    lrprfhitrrsHITReviewPolicy,
    lrprfhitrrsHITReviewReport,
    lrprfhitrrsNextToken,
    lrprfhitrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListReviewPolicyResultsForHIT' smart constructor.
data ListReviewPolicyResultsForHIT = ListReviewPolicyResultsForHIT'
  { -- | The unique identifier of the HIT to retrieve review results for.
    hITId :: Types.HITId,
    -- | Limit the number of results returned.
    maxResults :: Core.Maybe Core.Natural,
    -- | Pagination token
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | The Policy Level(s) to retrieve review results for - HIT or Assignment. If omitted, the default behavior is to retrieve all data for both policy levels. For a list of all the described policies, see Review Policies.
    policyLevels :: Core.Maybe [Types.ReviewPolicyLevel],
    -- | Specify if the operation should retrieve a list of the actions taken executing the Review Policies and their outcomes.
    retrieveActions :: Core.Maybe Core.Bool,
    -- | Specify if the operation should retrieve a list of the results computed by the Review Policies.
    retrieveResults :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListReviewPolicyResultsForHIT' value with any optional fields omitted.
mkListReviewPolicyResultsForHIT ::
  -- | 'hITId'
  Types.HITId ->
  ListReviewPolicyResultsForHIT
mkListReviewPolicyResultsForHIT hITId =
  ListReviewPolicyResultsForHIT'
    { hITId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      policyLevels = Core.Nothing,
      retrieveActions = Core.Nothing,
      retrieveResults = Core.Nothing
    }

-- | The unique identifier of the HIT to retrieve review results for.
--
-- /Note:/ Consider using 'hITId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrprfhitHITId :: Lens.Lens' ListReviewPolicyResultsForHIT Types.HITId
lrprfhitHITId = Lens.field @"hITId"
{-# DEPRECATED lrprfhitHITId "Use generic-lens or generic-optics with 'hITId' instead." #-}

-- | Limit the number of results returned.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrprfhitMaxResults :: Lens.Lens' ListReviewPolicyResultsForHIT (Core.Maybe Core.Natural)
lrprfhitMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lrprfhitMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Pagination token
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrprfhitNextToken :: Lens.Lens' ListReviewPolicyResultsForHIT (Core.Maybe Types.PaginationToken)
lrprfhitNextToken = Lens.field @"nextToken"
{-# DEPRECATED lrprfhitNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The Policy Level(s) to retrieve review results for - HIT or Assignment. If omitted, the default behavior is to retrieve all data for both policy levels. For a list of all the described policies, see Review Policies.
--
-- /Note:/ Consider using 'policyLevels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrprfhitPolicyLevels :: Lens.Lens' ListReviewPolicyResultsForHIT (Core.Maybe [Types.ReviewPolicyLevel])
lrprfhitPolicyLevels = Lens.field @"policyLevels"
{-# DEPRECATED lrprfhitPolicyLevels "Use generic-lens or generic-optics with 'policyLevels' instead." #-}

-- | Specify if the operation should retrieve a list of the actions taken executing the Review Policies and their outcomes.
--
-- /Note:/ Consider using 'retrieveActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrprfhitRetrieveActions :: Lens.Lens' ListReviewPolicyResultsForHIT (Core.Maybe Core.Bool)
lrprfhitRetrieveActions = Lens.field @"retrieveActions"
{-# DEPRECATED lrprfhitRetrieveActions "Use generic-lens or generic-optics with 'retrieveActions' instead." #-}

-- | Specify if the operation should retrieve a list of the results computed by the Review Policies.
--
-- /Note:/ Consider using 'retrieveResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrprfhitRetrieveResults :: Lens.Lens' ListReviewPolicyResultsForHIT (Core.Maybe Core.Bool)
lrprfhitRetrieveResults = Lens.field @"retrieveResults"
{-# DEPRECATED lrprfhitRetrieveResults "Use generic-lens or generic-optics with 'retrieveResults' instead." #-}

instance Core.FromJSON ListReviewPolicyResultsForHIT where
  toJSON ListReviewPolicyResultsForHIT {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("HITId" Core..= hITId),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("PolicyLevels" Core..=) Core.<$> policyLevels,
            ("RetrieveActions" Core..=) Core.<$> retrieveActions,
            ("RetrieveResults" Core..=) Core.<$> retrieveResults
          ]
      )

instance Core.AWSRequest ListReviewPolicyResultsForHIT where
  type
    Rs ListReviewPolicyResultsForHIT =
      ListReviewPolicyResultsForHITResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "MTurkRequesterServiceV20170117.ListReviewPolicyResultsForHIT"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListReviewPolicyResultsForHITResponse'
            Core.<$> (x Core..:? "AssignmentReviewPolicy")
            Core.<*> (x Core..:? "AssignmentReviewReport")
            Core.<*> (x Core..:? "HITId")
            Core.<*> (x Core..:? "HITReviewPolicy")
            Core.<*> (x Core..:? "HITReviewReport")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListReviewPolicyResultsForHITResponse' smart constructor.
data ListReviewPolicyResultsForHITResponse = ListReviewPolicyResultsForHITResponse'
  { -- | The name of the Assignment-level Review Policy. This contains only the PolicyName element.
    assignmentReviewPolicy :: Core.Maybe Types.ReviewPolicy,
    -- | Contains both ReviewResult and ReviewAction elements for an Assignment.
    assignmentReviewReport :: Core.Maybe Types.ReviewReport,
    -- | The HITId of the HIT for which results have been returned.
    hITId :: Core.Maybe Types.HITId,
    -- | The name of the HIT-level Review Policy. This contains only the PolicyName element.
    hITReviewPolicy :: Core.Maybe Types.ReviewPolicy,
    -- | Contains both ReviewResult and ReviewAction elements for a particular HIT.
    hITReviewReport :: Core.Maybe Types.ReviewReport,
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListReviewPolicyResultsForHITResponse' value with any optional fields omitted.
mkListReviewPolicyResultsForHITResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListReviewPolicyResultsForHITResponse
mkListReviewPolicyResultsForHITResponse responseStatus =
  ListReviewPolicyResultsForHITResponse'
    { assignmentReviewPolicy =
        Core.Nothing,
      assignmentReviewReport = Core.Nothing,
      hITId = Core.Nothing,
      hITReviewPolicy = Core.Nothing,
      hITReviewReport = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The name of the Assignment-level Review Policy. This contains only the PolicyName element.
--
-- /Note:/ Consider using 'assignmentReviewPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrprfhitrrsAssignmentReviewPolicy :: Lens.Lens' ListReviewPolicyResultsForHITResponse (Core.Maybe Types.ReviewPolicy)
lrprfhitrrsAssignmentReviewPolicy = Lens.field @"assignmentReviewPolicy"
{-# DEPRECATED lrprfhitrrsAssignmentReviewPolicy "Use generic-lens or generic-optics with 'assignmentReviewPolicy' instead." #-}

-- | Contains both ReviewResult and ReviewAction elements for an Assignment.
--
-- /Note:/ Consider using 'assignmentReviewReport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrprfhitrrsAssignmentReviewReport :: Lens.Lens' ListReviewPolicyResultsForHITResponse (Core.Maybe Types.ReviewReport)
lrprfhitrrsAssignmentReviewReport = Lens.field @"assignmentReviewReport"
{-# DEPRECATED lrprfhitrrsAssignmentReviewReport "Use generic-lens or generic-optics with 'assignmentReviewReport' instead." #-}

-- | The HITId of the HIT for which results have been returned.
--
-- /Note:/ Consider using 'hITId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrprfhitrrsHITId :: Lens.Lens' ListReviewPolicyResultsForHITResponse (Core.Maybe Types.HITId)
lrprfhitrrsHITId = Lens.field @"hITId"
{-# DEPRECATED lrprfhitrrsHITId "Use generic-lens or generic-optics with 'hITId' instead." #-}

-- | The name of the HIT-level Review Policy. This contains only the PolicyName element.
--
-- /Note:/ Consider using 'hITReviewPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrprfhitrrsHITReviewPolicy :: Lens.Lens' ListReviewPolicyResultsForHITResponse (Core.Maybe Types.ReviewPolicy)
lrprfhitrrsHITReviewPolicy = Lens.field @"hITReviewPolicy"
{-# DEPRECATED lrprfhitrrsHITReviewPolicy "Use generic-lens or generic-optics with 'hITReviewPolicy' instead." #-}

-- | Contains both ReviewResult and ReviewAction elements for a particular HIT.
--
-- /Note:/ Consider using 'hITReviewReport' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrprfhitrrsHITReviewReport :: Lens.Lens' ListReviewPolicyResultsForHITResponse (Core.Maybe Types.ReviewReport)
lrprfhitrrsHITReviewReport = Lens.field @"hITReviewReport"
{-# DEPRECATED lrprfhitrrsHITReviewReport "Use generic-lens or generic-optics with 'hITReviewReport' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrprfhitrrsNextToken :: Lens.Lens' ListReviewPolicyResultsForHITResponse (Core.Maybe Types.PaginationToken)
lrprfhitrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lrprfhitrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrprfhitrrsResponseStatus :: Lens.Lens' ListReviewPolicyResultsForHITResponse Core.Int
lrprfhitrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lrprfhitrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
