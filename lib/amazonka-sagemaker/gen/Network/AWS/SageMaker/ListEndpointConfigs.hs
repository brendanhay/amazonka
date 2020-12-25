{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListEndpointConfigs
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists endpoint configurations.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListEndpointConfigs
  ( -- * Creating a request
    ListEndpointConfigs (..),
    mkListEndpointConfigs,

    -- ** Request lenses
    lecCreationTimeAfter,
    lecCreationTimeBefore,
    lecMaxResults,
    lecNameContains,
    lecNextToken,
    lecSortBy,
    lecSortOrder,

    -- * Destructuring the response
    ListEndpointConfigsResponse (..),
    mkListEndpointConfigsResponse,

    -- ** Response lenses
    lecrrsEndpointConfigs,
    lecrrsNextToken,
    lecrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkListEndpointConfigs' smart constructor.
data ListEndpointConfigs = ListEndpointConfigs'
  { -- | A filter that returns only endpoint configurations with a creation time greater than or equal to the specified time (timestamp).
    creationTimeAfter :: Core.Maybe Core.NominalDiffTime,
    -- | A filter that returns only endpoint configurations created before the specified time (timestamp).
    creationTimeBefore :: Core.Maybe Core.NominalDiffTime,
    -- | The maximum number of training jobs to return in the response.
    maxResults :: Core.Maybe Core.Natural,
    -- | A string in the endpoint configuration name. This filter returns only endpoint configurations whose name contains the specified string.
    nameContains :: Core.Maybe Types.EndpointConfigNameContains,
    -- | If the result of the previous @ListEndpointConfig@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of endpoint configurations, use the token in the next request.
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | The field to sort results by. The default is @CreationTime@ .
    sortBy :: Core.Maybe Types.EndpointConfigSortKey,
    -- | The sort order for results. The default is @Descending@ .
    sortOrder :: Core.Maybe Types.OrderKey
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListEndpointConfigs' value with any optional fields omitted.
mkListEndpointConfigs ::
  ListEndpointConfigs
mkListEndpointConfigs =
  ListEndpointConfigs'
    { creationTimeAfter = Core.Nothing,
      creationTimeBefore = Core.Nothing,
      maxResults = Core.Nothing,
      nameContains = Core.Nothing,
      nextToken = Core.Nothing,
      sortBy = Core.Nothing,
      sortOrder = Core.Nothing
    }

-- | A filter that returns only endpoint configurations with a creation time greater than or equal to the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lecCreationTimeAfter :: Lens.Lens' ListEndpointConfigs (Core.Maybe Core.NominalDiffTime)
lecCreationTimeAfter = Lens.field @"creationTimeAfter"
{-# DEPRECATED lecCreationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead." #-}

-- | A filter that returns only endpoint configurations created before the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lecCreationTimeBefore :: Lens.Lens' ListEndpointConfigs (Core.Maybe Core.NominalDiffTime)
lecCreationTimeBefore = Lens.field @"creationTimeBefore"
{-# DEPRECATED lecCreationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead." #-}

-- | The maximum number of training jobs to return in the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lecMaxResults :: Lens.Lens' ListEndpointConfigs (Core.Maybe Core.Natural)
lecMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lecMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A string in the endpoint configuration name. This filter returns only endpoint configurations whose name contains the specified string.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lecNameContains :: Lens.Lens' ListEndpointConfigs (Core.Maybe Types.EndpointConfigNameContains)
lecNameContains = Lens.field @"nameContains"
{-# DEPRECATED lecNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | If the result of the previous @ListEndpointConfig@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of endpoint configurations, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lecNextToken :: Lens.Lens' ListEndpointConfigs (Core.Maybe Types.PaginationToken)
lecNextToken = Lens.field @"nextToken"
{-# DEPRECATED lecNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The field to sort results by. The default is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lecSortBy :: Lens.Lens' ListEndpointConfigs (Core.Maybe Types.EndpointConfigSortKey)
lecSortBy = Lens.field @"sortBy"
{-# DEPRECATED lecSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

-- | The sort order for results. The default is @Descending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lecSortOrder :: Lens.Lens' ListEndpointConfigs (Core.Maybe Types.OrderKey)
lecSortOrder = Lens.field @"sortOrder"
{-# DEPRECATED lecSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

instance Core.FromJSON ListEndpointConfigs where
  toJSON ListEndpointConfigs {..} =
    Core.object
      ( Core.catMaybes
          [ ("CreationTimeAfter" Core..=) Core.<$> creationTimeAfter,
            ("CreationTimeBefore" Core..=) Core.<$> creationTimeBefore,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NameContains" Core..=) Core.<$> nameContains,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("SortBy" Core..=) Core.<$> sortBy,
            ("SortOrder" Core..=) Core.<$> sortOrder
          ]
      )

instance Core.AWSRequest ListEndpointConfigs where
  type Rs ListEndpointConfigs = ListEndpointConfigsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.ListEndpointConfigs")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEndpointConfigsResponse'
            Core.<$> (x Core..:? "EndpointConfigs" Core..!= Core.mempty)
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListEndpointConfigs where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^. Lens.field @"endpointConfigs") =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListEndpointConfigsResponse' smart constructor.
data ListEndpointConfigsResponse = ListEndpointConfigsResponse'
  { -- | An array of endpoint configurations.
    endpointConfigs :: [Types.EndpointConfigSummary],
    -- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of endpoint configurations, use it in the subsequent request
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListEndpointConfigsResponse' value with any optional fields omitted.
mkListEndpointConfigsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListEndpointConfigsResponse
mkListEndpointConfigsResponse responseStatus =
  ListEndpointConfigsResponse'
    { endpointConfigs = Core.mempty,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | An array of endpoint configurations.
--
-- /Note:/ Consider using 'endpointConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lecrrsEndpointConfigs :: Lens.Lens' ListEndpointConfigsResponse [Types.EndpointConfigSummary]
lecrrsEndpointConfigs = Lens.field @"endpointConfigs"
{-# DEPRECATED lecrrsEndpointConfigs "Use generic-lens or generic-optics with 'endpointConfigs' instead." #-}

-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of endpoint configurations, use it in the subsequent request
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lecrrsNextToken :: Lens.Lens' ListEndpointConfigsResponse (Core.Maybe Types.PaginationToken)
lecrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lecrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lecrrsResponseStatus :: Lens.Lens' ListEndpointConfigsResponse Core.Int
lecrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lecrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
