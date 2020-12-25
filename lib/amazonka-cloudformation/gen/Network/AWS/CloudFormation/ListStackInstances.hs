{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.ListStackInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns summary information about stack instances that are associated with the specified stack set. You can filter for stack instances that are associated with a specific AWS account name or Region, or that have a specific status.
--
-- This operation returns paginated results.
module Network.AWS.CloudFormation.ListStackInstances
  ( -- * Creating a request
    ListStackInstances (..),
    mkListStackInstances,

    -- ** Request lenses
    lsiStackSetName,
    lsiFilters,
    lsiMaxResults,
    lsiNextToken,
    lsiStackInstanceAccount,
    lsiStackInstanceRegion,

    -- * Destructuring the response
    ListStackInstancesResponse (..),
    mkListStackInstancesResponse,

    -- ** Response lenses
    lsirrsNextToken,
    lsirrsSummaries,
    lsirrsResponseStatus,
  )
where

import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListStackInstances' smart constructor.
data ListStackInstances = ListStackInstances'
  { -- | The name or unique ID of the stack set that you want to list stack instances for.
    stackSetName :: Types.StackSetName,
    -- | The status that stack instances are filtered by.
    filters :: Core.Maybe [Types.StackInstanceFilter],
    -- | The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | If the previous request didn't return all of the remaining results, the response's @NextToken@ parameter value is set to a token. To retrieve the next set of results, call @ListStackInstances@ again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
    nextToken :: Core.Maybe Types.NextToken,
    -- | The name of the AWS account that you want to list stack instances for.
    stackInstanceAccount :: Core.Maybe Types.StackInstanceAccount,
    -- | The name of the Region where you want to list stack instances.
    stackInstanceRegion :: Core.Maybe Types.StackInstanceRegion
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListStackInstances' value with any optional fields omitted.
mkListStackInstances ::
  -- | 'stackSetName'
  Types.StackSetName ->
  ListStackInstances
mkListStackInstances stackSetName =
  ListStackInstances'
    { stackSetName,
      filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      stackInstanceAccount = Core.Nothing,
      stackInstanceRegion = Core.Nothing
    }

-- | The name or unique ID of the stack set that you want to list stack instances for.
--
-- /Note:/ Consider using 'stackSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsiStackSetName :: Lens.Lens' ListStackInstances Types.StackSetName
lsiStackSetName = Lens.field @"stackSetName"
{-# DEPRECATED lsiStackSetName "Use generic-lens or generic-optics with 'stackSetName' instead." #-}

-- | The status that stack instances are filtered by.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsiFilters :: Lens.Lens' ListStackInstances (Core.Maybe [Types.StackInstanceFilter])
lsiFilters = Lens.field @"filters"
{-# DEPRECATED lsiFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of results to be returned with a single call. If the number of available results exceeds this maximum, the response includes a @NextToken@ value that you can assign to the @NextToken@ request parameter to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsiMaxResults :: Lens.Lens' ListStackInstances (Core.Maybe Core.Natural)
lsiMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lsiMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | If the previous request didn't return all of the remaining results, the response's @NextToken@ parameter value is set to a token. To retrieve the next set of results, call @ListStackInstances@ again and assign that token to the request object's @NextToken@ parameter. If there are no remaining results, the previous response object's @NextToken@ parameter is set to @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsiNextToken :: Lens.Lens' ListStackInstances (Core.Maybe Types.NextToken)
lsiNextToken = Lens.field @"nextToken"
{-# DEPRECATED lsiNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name of the AWS account that you want to list stack instances for.
--
-- /Note:/ Consider using 'stackInstanceAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsiStackInstanceAccount :: Lens.Lens' ListStackInstances (Core.Maybe Types.StackInstanceAccount)
lsiStackInstanceAccount = Lens.field @"stackInstanceAccount"
{-# DEPRECATED lsiStackInstanceAccount "Use generic-lens or generic-optics with 'stackInstanceAccount' instead." #-}

-- | The name of the Region where you want to list stack instances.
--
-- /Note:/ Consider using 'stackInstanceRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsiStackInstanceRegion :: Lens.Lens' ListStackInstances (Core.Maybe Types.StackInstanceRegion)
lsiStackInstanceRegion = Lens.field @"stackInstanceRegion"
{-# DEPRECATED lsiStackInstanceRegion "Use generic-lens or generic-optics with 'stackInstanceRegion' instead." #-}

instance Core.AWSRequest ListStackInstances where
  type Rs ListStackInstances = ListStackInstancesResponse
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
            ( Core.pure ("Action", "ListStackInstances")
                Core.<> (Core.pure ("Version", "2010-05-15"))
                Core.<> (Core.toQueryValue "StackSetName" stackSetName)
                Core.<> ( Core.toQueryValue
                            "Filters"
                            (Core.toQueryList "member" Core.<$> filters)
                        )
                Core.<> (Core.toQueryValue "MaxResults" Core.<$> maxResults)
                Core.<> (Core.toQueryValue "NextToken" Core.<$> nextToken)
                Core.<> ( Core.toQueryValue "StackInstanceAccount"
                            Core.<$> stackInstanceAccount
                        )
                Core.<> ( Core.toQueryValue "StackInstanceRegion"
                            Core.<$> stackInstanceRegion
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "ListStackInstancesResult"
      ( \s h x ->
          ListStackInstancesResponse'
            Core.<$> (x Core..@? "NextToken")
            Core.<*> (x Core..@? "Summaries" Core..<@> Core.parseXMLList "member")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListStackInstances where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"summaries" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListStackInstancesResponse' smart constructor.
data ListStackInstancesResponse = ListStackInstancesResponse'
  { -- | If the request doesn't return all of the remaining results, @NextToken@ is set to a token. To retrieve the next set of results, call @ListStackInstances@ again and assign that token to the request object's @NextToken@ parameter. If the request returns all results, @NextToken@ is set to @null@ .
    nextToken :: Core.Maybe Types.NextToken,
    -- | A list of @StackInstanceSummary@ structures that contain information about the specified stack instances.
    summaries :: Core.Maybe [Types.StackInstanceSummary],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListStackInstancesResponse' value with any optional fields omitted.
mkListStackInstancesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListStackInstancesResponse
mkListStackInstancesResponse responseStatus =
  ListStackInstancesResponse'
    { nextToken = Core.Nothing,
      summaries = Core.Nothing,
      responseStatus
    }

-- | If the request doesn't return all of the remaining results, @NextToken@ is set to a token. To retrieve the next set of results, call @ListStackInstances@ again and assign that token to the request object's @NextToken@ parameter. If the request returns all results, @NextToken@ is set to @null@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsirrsNextToken :: Lens.Lens' ListStackInstancesResponse (Core.Maybe Types.NextToken)
lsirrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lsirrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of @StackInstanceSummary@ structures that contain information about the specified stack instances.
--
-- /Note:/ Consider using 'summaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsirrsSummaries :: Lens.Lens' ListStackInstancesResponse (Core.Maybe [Types.StackInstanceSummary])
lsirrsSummaries = Lens.field @"summaries"
{-# DEPRECATED lsirrsSummaries "Use generic-lens or generic-optics with 'summaries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsirrsResponseStatus :: Lens.Lens' ListStackInstancesResponse Core.Int
lsirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lsirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
