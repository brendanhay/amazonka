{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.ListOperations
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists operations that match the criteria that you specify.
--
-- This operation returns paginated results.
module Network.AWS.Route53AutoNaming.ListOperations
  ( -- * Creating a request
    ListOperations (..),
    mkListOperations,

    -- ** Request lenses
    loFilters,
    loMaxResults,
    loNextToken,

    -- * Destructuring the response
    ListOperationsResponse (..),
    mkListOperationsResponse,

    -- ** Response lenses
    lorrsNextToken,
    lorrsOperations,
    lorrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Route53AutoNaming.Types as Types

-- | /See:/ 'mkListOperations' smart constructor.
data ListOperations = ListOperations'
  { -- | A complex type that contains specifications for the operations that you want to list, for example, operations that you started between a specified start date and end date.
    --
    -- If you specify more than one filter, an operation must match all filters to be returned by @ListOperations@ .
    filters :: Core.Maybe [Types.OperationFilter],
    -- | The maximum number of items that you want AWS Cloud Map to return in the response to a @ListOperations@ request. If you don't specify a value for @MaxResults@ , AWS Cloud Map returns up to 100 operations.
    maxResults :: Core.Maybe Core.Natural,
    -- | For the first @ListOperations@ request, omit this value.
    --
    -- If the response contains @NextToken@ , submit another @ListOperations@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListOperations' value with any optional fields omitted.
mkListOperations ::
  ListOperations
mkListOperations =
  ListOperations'
    { filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | A complex type that contains specifications for the operations that you want to list, for example, operations that you started between a specified start date and end date.
--
-- If you specify more than one filter, an operation must match all filters to be returned by @ListOperations@ .
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loFilters :: Lens.Lens' ListOperations (Core.Maybe [Types.OperationFilter])
loFilters = Lens.field @"filters"
{-# DEPRECATED loFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of items that you want AWS Cloud Map to return in the response to a @ListOperations@ request. If you don't specify a value for @MaxResults@ , AWS Cloud Map returns up to 100 operations.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loMaxResults :: Lens.Lens' ListOperations (Core.Maybe Core.Natural)
loMaxResults = Lens.field @"maxResults"
{-# DEPRECATED loMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | For the first @ListOperations@ request, omit this value.
--
-- If the response contains @NextToken@ , submit another @ListOperations@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loNextToken :: Lens.Lens' ListOperations (Core.Maybe Types.NextToken)
loNextToken = Lens.field @"nextToken"
{-# DEPRECATED loNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListOperations where
  toJSON ListOperations {..} =
    Core.object
      ( Core.catMaybes
          [ ("Filters" Core..=) Core.<$> filters,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListOperations where
  type Rs ListOperations = ListOperationsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "Route53AutoNaming_v20170314.ListOperations")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOperationsResponse'
            Core.<$> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "Operations")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListOperations where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"operations" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListOperationsResponse' smart constructor.
data ListOperationsResponse = ListOperationsResponse'
  { -- | If the response contains @NextToken@ , submit another @ListOperations@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
    nextToken :: Core.Maybe Types.NextToken,
    -- | Summary information about the operations that match the specified criteria.
    operations :: Core.Maybe [Types.OperationSummary],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListOperationsResponse' value with any optional fields omitted.
mkListOperationsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListOperationsResponse
mkListOperationsResponse responseStatus =
  ListOperationsResponse'
    { nextToken = Core.Nothing,
      operations = Core.Nothing,
      responseStatus
    }

-- | If the response contains @NextToken@ , submit another @ListOperations@ request to get the next group of results. Specify the value of @NextToken@ from the previous response in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorrsNextToken :: Lens.Lens' ListOperationsResponse (Core.Maybe Types.NextToken)
lorrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lorrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Summary information about the operations that match the specified criteria.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorrsOperations :: Lens.Lens' ListOperationsResponse (Core.Maybe [Types.OperationSummary])
lorrsOperations = Lens.field @"operations"
{-# DEPRECATED lorrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lorrsResponseStatus :: Lens.Lens' ListOperationsResponse Core.Int
lorrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lorrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
