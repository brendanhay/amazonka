{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.ListStackSetOperations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summary information about operations performed on a stack set.
--
-- This operation returns paginated results.
module Network.AWS.CloudFormation.ListStackSetOperations
  ( -- * Creating a request
    ListStackSetOperations (..),
    mkListStackSetOperations,

    -- ** Request lenses
    lssoStackSetName,
    lssoMaxResults,
    lssoNextToken,

    -- * Destructuring the response
    ListStackSetOperationsResponse (..),
    mkListStackSetOperationsResponse,

    -- ** Response lenses
    lssorrsNextToken,
    lssorrsSummaries,
    lssorrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListStackSetOperations' smart constructor.
data ListStackSetOperations = ListStackSetOperations'
  { -- | The name or unique ID of the stack set that you want to get operation summaries for.
    stackSetName :: Types.StackSetName,
    -- | The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | If the previous paginated request didn't return all of the remaining results, the response object's @NextToken@ parameter value is set to a token. To retrieve the next set of results, call @ListStackSetOperations@ again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListStackSetOperations' value with any optional fields omitted.
mkListStackSetOperations ::
  -- | 'stackSetName'
  Types.StackSetName ->
  ListStackSetOperations
mkListStackSetOperations stackSetName =
  ListStackSetOperations'
    { stackSetName,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The name or unique ID of the stack set that you want to get operation summaries for.
--
-- /Note:/ Consider using 'stackSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssoStackSetName :: Lens.Lens' ListStackSetOperations Types.StackSetName
lssoStackSetName = Lens.field @"stackSetName"
{-# DEPRECATED lssoStackSetName "Use generic-lens or generic-optics with 'stackSetName' instead." #-}

-- | The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssoMaxResults :: Lens.Lens' ListStackSetOperations (Core.Maybe Core.Natural)
lssoMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lssoMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | If the previous paginated request didn't return all of the remaining results, the response object's @NextToken@ parameter value is set to a token. To retrieve the next set of results, call @ListStackSetOperations@ again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssoNextToken :: Lens.Lens' ListStackSetOperations (Core.Maybe Types.NextToken)
lssoNextToken = Lens.field @"nextToken"
{-# DEPRECATED lssoNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListStackSetOperations where
  type Rs ListStackSetOperations = ListStackSetOperationsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "ListStackSetOperations")
                Core.<> (Core.pure ("Version", "2010-05-15"))
                Core.<> (Core.toQueryValue "StackSetName" stackSetName)
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
            )
      }
  response =
    Response.receiveXMLWrapper
      "ListStackSetOperationsResult"
      ( \s h x ->
          ListStackSetOperationsResponse'
            Core.<$> (x Core..@? "NextToken")
            Core.<*> (x Core..@? "Summaries" Core..<@> Core.parseXMLList "member")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListStackSetOperations where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"summaries" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListStackSetOperationsResponse' smart constructor.
data ListStackSetOperationsResponse = ListStackSetOperationsResponse'
  { -- | If the request doesn't return all results, @NextToken@ is set to a token. To retrieve the next set of results, call @ListOperationResults@ again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, @NextToken@ is set to @null@ .
    nextToken :: Core.Maybe Types.NextToken,
    -- | A list of @StackSetOperationSummary@ structures that contain summary information about operations for the specified stack set.
    summaries :: Core.Maybe [Types.StackSetOperationSummary],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListStackSetOperationsResponse' value with any optional fields omitted.
mkListStackSetOperationsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListStackSetOperationsResponse
mkListStackSetOperationsResponse responseStatus =
  ListStackSetOperationsResponse'
    { nextToken = Core.Nothing,
      summaries = Core.Nothing,
      responseStatus
    }

-- | If the request doesn't return all results, @NextToken@ is set to a token. To retrieve the next set of results, call @ListOperationResults@ again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, @NextToken@ is set to @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssorrsNextToken :: Lens.Lens' ListStackSetOperationsResponse (Core.Maybe Types.NextToken)
lssorrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lssorrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of @StackSetOperationSummary@ structures that contain summary information about operations for the specified stack set.
--
-- /Note:/ Consider using 'summaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssorrsSummaries :: Lens.Lens' ListStackSetOperationsResponse (Core.Maybe [Types.StackSetOperationSummary])
lssorrsSummaries = Lens.field @"summaries"
{-# DEPRECATED lssorrsSummaries "Use generic-lens or generic-optics with 'summaries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lssorrsResponseStatus :: Lens.Lens' ListStackSetOperationsResponse Core.Int
lssorrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lssorrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
