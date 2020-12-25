{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.ListBuildBatches
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the identifiers of your build batches in the current region.
--
-- This operation returns paginated results.
module Network.AWS.CodeBuild.ListBuildBatches
  ( -- * Creating a request
    ListBuildBatches (..),
    mkListBuildBatches,

    -- ** Request lenses
    lbbFilter,
    lbbMaxResults,
    lbbNextToken,
    lbbSortOrder,

    -- * Destructuring the response
    ListBuildBatchesResponse (..),
    mkListBuildBatchesResponse,

    -- ** Response lenses
    lbbrrsIds,
    lbbrrsNextToken,
    lbbrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListBuildBatches' smart constructor.
data ListBuildBatches = ListBuildBatches'
  { -- | A @BuildBatchFilter@ object that specifies the filters for the search.
    filter :: Core.Maybe Types.BuildBatchFilter,
    -- | The maximum number of results to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | The @nextToken@ value returned from a previous call to @ListBuildBatches@ . This specifies the next item to return. To return the beginning of the list, exclude this parameter.
    nextToken :: Core.Maybe Types.String,
    -- | Specifies the sort order of the returned items. Valid values include:
    --
    --
    --     * @ASCENDING@ : List the batch build identifiers in ascending order by identifier.
    --
    --
    --     * @DESCENDING@ : List the batch build identifiers in descending order by identifier.
    sortOrder :: Core.Maybe Types.SortOrderType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListBuildBatches' value with any optional fields omitted.
mkListBuildBatches ::
  ListBuildBatches
mkListBuildBatches =
  ListBuildBatches'
    { filter = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      sortOrder = Core.Nothing
    }

-- | A @BuildBatchFilter@ object that specifies the filters for the search.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbbFilter :: Lens.Lens' ListBuildBatches (Core.Maybe Types.BuildBatchFilter)
lbbFilter = Lens.field @"filter"
{-# DEPRECATED lbbFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The maximum number of results to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbbMaxResults :: Lens.Lens' ListBuildBatches (Core.Maybe Core.Natural)
lbbMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lbbMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The @nextToken@ value returned from a previous call to @ListBuildBatches@ . This specifies the next item to return. To return the beginning of the list, exclude this parameter.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbbNextToken :: Lens.Lens' ListBuildBatches (Core.Maybe Types.String)
lbbNextToken = Lens.field @"nextToken"
{-# DEPRECATED lbbNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Specifies the sort order of the returned items. Valid values include:
--
--
--     * @ASCENDING@ : List the batch build identifiers in ascending order by identifier.
--
--
--     * @DESCENDING@ : List the batch build identifiers in descending order by identifier.
--
--
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbbSortOrder :: Lens.Lens' ListBuildBatches (Core.Maybe Types.SortOrderType)
lbbSortOrder = Lens.field @"sortOrder"
{-# DEPRECATED lbbSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

instance Core.FromJSON ListBuildBatches where
  toJSON ListBuildBatches {..} =
    Core.object
      ( Core.catMaybes
          [ ("filter" Core..=) Core.<$> filter,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("nextToken" Core..=) Core.<$> nextToken,
            ("sortOrder" Core..=) Core.<$> sortOrder
          ]
      )

instance Core.AWSRequest ListBuildBatches where
  type Rs ListBuildBatches = ListBuildBatchesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeBuild_20161006.ListBuildBatches")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBuildBatchesResponse'
            Core.<$> (x Core..:? "ids")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListBuildBatches where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"ids" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListBuildBatchesResponse' smart constructor.
data ListBuildBatchesResponse = ListBuildBatchesResponse'
  { -- | An array of strings that contains the batch build identifiers.
    ids :: Core.Maybe [Types.NonEmptyString],
    -- | If there are more items to return, this contains a token that is passed to a subsequent call to @ListBuildBatches@ to retrieve the next set of items.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListBuildBatchesResponse' value with any optional fields omitted.
mkListBuildBatchesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListBuildBatchesResponse
mkListBuildBatchesResponse responseStatus =
  ListBuildBatchesResponse'
    { ids = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | An array of strings that contains the batch build identifiers.
--
-- /Note:/ Consider using 'ids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbbrrsIds :: Lens.Lens' ListBuildBatchesResponse (Core.Maybe [Types.NonEmptyString])
lbbrrsIds = Lens.field @"ids"
{-# DEPRECATED lbbrrsIds "Use generic-lens or generic-optics with 'ids' instead." #-}

-- | If there are more items to return, this contains a token that is passed to a subsequent call to @ListBuildBatches@ to retrieve the next set of items.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbbrrsNextToken :: Lens.Lens' ListBuildBatchesResponse (Core.Maybe Types.String)
lbbrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lbbrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbbrrsResponseStatus :: Lens.Lens' ListBuildBatchesResponse Core.Int
lbbrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lbbrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
