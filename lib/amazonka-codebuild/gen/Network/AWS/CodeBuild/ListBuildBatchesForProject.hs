{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.ListBuildBatchesForProject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the identifiers of the build batches for a specific project.
--
-- This operation returns paginated results.
module Network.AWS.CodeBuild.ListBuildBatchesForProject
  ( -- * Creating a request
    ListBuildBatchesForProject (..),
    mkListBuildBatchesForProject,

    -- ** Request lenses
    lbbfpFilter,
    lbbfpMaxResults,
    lbbfpNextToken,
    lbbfpProjectName,
    lbbfpSortOrder,

    -- * Destructuring the response
    ListBuildBatchesForProjectResponse (..),
    mkListBuildBatchesForProjectResponse,

    -- ** Response lenses
    lbbfprrsIds,
    lbbfprrsNextToken,
    lbbfprrsResponseStatus,
  )
where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListBuildBatchesForProject' smart constructor.
data ListBuildBatchesForProject = ListBuildBatchesForProject'
  { -- | A @BuildBatchFilter@ object that specifies the filters for the search.
    filter :: Core.Maybe Types.BuildBatchFilter,
    -- | The maximum number of results to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | The @nextToken@ value returned from a previous call to @ListBuildBatchesForProject@ . This specifies the next item to return. To return the beginning of the list, exclude this parameter.
    nextToken :: Core.Maybe Types.String,
    -- | The name of the project.
    projectName :: Core.Maybe Types.NonEmptyString,
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

-- | Creates a 'ListBuildBatchesForProject' value with any optional fields omitted.
mkListBuildBatchesForProject ::
  ListBuildBatchesForProject
mkListBuildBatchesForProject =
  ListBuildBatchesForProject'
    { filter = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      projectName = Core.Nothing,
      sortOrder = Core.Nothing
    }

-- | A @BuildBatchFilter@ object that specifies the filters for the search.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbbfpFilter :: Lens.Lens' ListBuildBatchesForProject (Core.Maybe Types.BuildBatchFilter)
lbbfpFilter = Lens.field @"filter"
{-# DEPRECATED lbbfpFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The maximum number of results to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbbfpMaxResults :: Lens.Lens' ListBuildBatchesForProject (Core.Maybe Core.Natural)
lbbfpMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lbbfpMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The @nextToken@ value returned from a previous call to @ListBuildBatchesForProject@ . This specifies the next item to return. To return the beginning of the list, exclude this parameter.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbbfpNextToken :: Lens.Lens' ListBuildBatchesForProject (Core.Maybe Types.String)
lbbfpNextToken = Lens.field @"nextToken"
{-# DEPRECATED lbbfpNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name of the project.
--
-- /Note:/ Consider using 'projectName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbbfpProjectName :: Lens.Lens' ListBuildBatchesForProject (Core.Maybe Types.NonEmptyString)
lbbfpProjectName = Lens.field @"projectName"
{-# DEPRECATED lbbfpProjectName "Use generic-lens or generic-optics with 'projectName' instead." #-}

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
lbbfpSortOrder :: Lens.Lens' ListBuildBatchesForProject (Core.Maybe Types.SortOrderType)
lbbfpSortOrder = Lens.field @"sortOrder"
{-# DEPRECATED lbbfpSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

instance Core.FromJSON ListBuildBatchesForProject where
  toJSON ListBuildBatchesForProject {..} =
    Core.object
      ( Core.catMaybes
          [ ("filter" Core..=) Core.<$> filter,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("nextToken" Core..=) Core.<$> nextToken,
            ("projectName" Core..=) Core.<$> projectName,
            ("sortOrder" Core..=) Core.<$> sortOrder
          ]
      )

instance Core.AWSRequest ListBuildBatchesForProject where
  type
    Rs ListBuildBatchesForProject =
      ListBuildBatchesForProjectResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodeBuild_20161006.ListBuildBatchesForProject")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBuildBatchesForProjectResponse'
            Core.<$> (x Core..:? "ids")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListBuildBatchesForProject where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"ids" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListBuildBatchesForProjectResponse' smart constructor.
data ListBuildBatchesForProjectResponse = ListBuildBatchesForProjectResponse'
  { -- | An array of strings that contains the batch build identifiers.
    ids :: Core.Maybe [Types.NonEmptyString],
    -- | If there are more items to return, this contains a token that is passed to a subsequent call to @ListBuildBatchesForProject@ to retrieve the next set of items.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListBuildBatchesForProjectResponse' value with any optional fields omitted.
mkListBuildBatchesForProjectResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListBuildBatchesForProjectResponse
mkListBuildBatchesForProjectResponse responseStatus =
  ListBuildBatchesForProjectResponse'
    { ids = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | An array of strings that contains the batch build identifiers.
--
-- /Note:/ Consider using 'ids' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbbfprrsIds :: Lens.Lens' ListBuildBatchesForProjectResponse (Core.Maybe [Types.NonEmptyString])
lbbfprrsIds = Lens.field @"ids"
{-# DEPRECATED lbbfprrsIds "Use generic-lens or generic-optics with 'ids' instead." #-}

-- | If there are more items to return, this contains a token that is passed to a subsequent call to @ListBuildBatchesForProject@ to retrieve the next set of items.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbbfprrsNextToken :: Lens.Lens' ListBuildBatchesForProjectResponse (Core.Maybe Types.String)
lbbfprrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lbbfprrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbbfprrsResponseStatus :: Lens.Lens' ListBuildBatchesForProjectResponse Core.Int
lbbfprrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lbbfprrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
