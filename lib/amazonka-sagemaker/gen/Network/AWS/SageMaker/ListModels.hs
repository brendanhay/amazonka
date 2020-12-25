{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListModels
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists models created with the 'CreateModel' API.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListModels
  ( -- * Creating a request
    ListModels (..),
    mkListModels,

    -- ** Request lenses
    lmCreationTimeAfter,
    lmCreationTimeBefore,
    lmMaxResults,
    lmNameContains,
    lmNextToken,
    lmSortBy,
    lmSortOrder,

    -- * Destructuring the response
    ListModelsResponse (..),
    mkListModelsResponse,

    -- ** Response lenses
    lmrrsModels,
    lmrrsNextToken,
    lmrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkListModels' smart constructor.
data ListModels = ListModels'
  { -- | A filter that returns only models with a creation time greater than or equal to the specified time (timestamp).
    creationTimeAfter :: Core.Maybe Core.NominalDiffTime,
    -- | A filter that returns only models created before the specified time (timestamp).
    creationTimeBefore :: Core.Maybe Core.NominalDiffTime,
    -- | The maximum number of models to return in the response.
    maxResults :: Core.Maybe Core.Natural,
    -- | A string in the training job name. This filter returns only models in the training job whose name contains the specified string.
    nameContains :: Core.Maybe Types.NameContains,
    -- | If the response to a previous @ListModels@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of models, use the token in the next request.
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | Sorts the list of results. The default is @CreationTime@ .
    sortBy :: Core.Maybe Types.ModelSortKey,
    -- | The sort order for results. The default is @Descending@ .
    sortOrder :: Core.Maybe Types.OrderKey
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListModels' value with any optional fields omitted.
mkListModels ::
  ListModels
mkListModels =
  ListModels'
    { creationTimeAfter = Core.Nothing,
      creationTimeBefore = Core.Nothing,
      maxResults = Core.Nothing,
      nameContains = Core.Nothing,
      nextToken = Core.Nothing,
      sortBy = Core.Nothing,
      sortOrder = Core.Nothing
    }

-- | A filter that returns only models with a creation time greater than or equal to the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmCreationTimeAfter :: Lens.Lens' ListModels (Core.Maybe Core.NominalDiffTime)
lmCreationTimeAfter = Lens.field @"creationTimeAfter"
{-# DEPRECATED lmCreationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead." #-}

-- | A filter that returns only models created before the specified time (timestamp).
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmCreationTimeBefore :: Lens.Lens' ListModels (Core.Maybe Core.NominalDiffTime)
lmCreationTimeBefore = Lens.field @"creationTimeBefore"
{-# DEPRECATED lmCreationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead." #-}

-- | The maximum number of models to return in the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmMaxResults :: Lens.Lens' ListModels (Core.Maybe Core.Natural)
lmMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lmMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A string in the training job name. This filter returns only models in the training job whose name contains the specified string.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmNameContains :: Lens.Lens' ListModels (Core.Maybe Types.NameContains)
lmNameContains = Lens.field @"nameContains"
{-# DEPRECATED lmNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | If the response to a previous @ListModels@ request was truncated, the response includes a @NextToken@ . To retrieve the next set of models, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmNextToken :: Lens.Lens' ListModels (Core.Maybe Types.PaginationToken)
lmNextToken = Lens.field @"nextToken"
{-# DEPRECATED lmNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Sorts the list of results. The default is @CreationTime@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmSortBy :: Lens.Lens' ListModels (Core.Maybe Types.ModelSortKey)
lmSortBy = Lens.field @"sortBy"
{-# DEPRECATED lmSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

-- | The sort order for results. The default is @Descending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmSortOrder :: Lens.Lens' ListModels (Core.Maybe Types.OrderKey)
lmSortOrder = Lens.field @"sortOrder"
{-# DEPRECATED lmSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

instance Core.FromJSON ListModels where
  toJSON ListModels {..} =
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

instance Core.AWSRequest ListModels where
  type Rs ListModels = ListModelsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.ListModels")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListModelsResponse'
            Core.<$> (x Core..:? "Models" Core..!= Core.mempty)
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListModels where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^. Lens.field @"models") = Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListModelsResponse' smart constructor.
data ListModelsResponse = ListModelsResponse'
  { -- | An array of @ModelSummary@ objects, each of which lists a model.
    models :: [Types.ModelSummary],
    -- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of models, use it in the subsequent request.
    nextToken :: Core.Maybe Types.PaginationToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListModelsResponse' value with any optional fields omitted.
mkListModelsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListModelsResponse
mkListModelsResponse responseStatus =
  ListModelsResponse'
    { models = Core.mempty,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | An array of @ModelSummary@ objects, each of which lists a model.
--
-- /Note:/ Consider using 'models' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmrrsModels :: Lens.Lens' ListModelsResponse [Types.ModelSummary]
lmrrsModels = Lens.field @"models"
{-# DEPRECATED lmrrsModels "Use generic-lens or generic-optics with 'models' instead." #-}

-- | If the response is truncated, Amazon SageMaker returns this token. To retrieve the next set of models, use it in the subsequent request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmrrsNextToken :: Lens.Lens' ListModelsResponse (Core.Maybe Types.PaginationToken)
lmrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lmrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmrrsResponseStatus :: Lens.Lens' ListModelsResponse Core.Int
lmrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lmrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
