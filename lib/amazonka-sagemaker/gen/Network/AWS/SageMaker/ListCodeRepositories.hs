{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.ListCodeRepositories
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the Git repositories in your account.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListCodeRepositories
  ( -- * Creating a request
    ListCodeRepositories (..),
    mkListCodeRepositories,

    -- ** Request lenses
    lcrCreationTimeAfter,
    lcrCreationTimeBefore,
    lcrLastModifiedTimeAfter,
    lcrLastModifiedTimeBefore,
    lcrMaxResults,
    lcrNameContains,
    lcrNextToken,
    lcrSortBy,
    lcrSortOrder,

    -- * Destructuring the response
    ListCodeRepositoriesResponse (..),
    mkListCodeRepositoriesResponse,

    -- ** Response lenses
    lcrrrsCodeRepositorySummaryList,
    lcrrrsNextToken,
    lcrrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkListCodeRepositories' smart constructor.
data ListCodeRepositories = ListCodeRepositories'
  { -- | A filter that returns only Git repositories that were created after the specified time.
    creationTimeAfter :: Core.Maybe Core.NominalDiffTime,
    -- | A filter that returns only Git repositories that were created before the specified time.
    creationTimeBefore :: Core.Maybe Core.NominalDiffTime,
    -- | A filter that returns only Git repositories that were last modified after the specified time.
    lastModifiedTimeAfter :: Core.Maybe Core.NominalDiffTime,
    -- | A filter that returns only Git repositories that were last modified before the specified time.
    lastModifiedTimeBefore :: Core.Maybe Core.NominalDiffTime,
    -- | The maximum number of Git repositories to return in the response.
    maxResults :: Core.Maybe Core.Natural,
    -- | A string in the Git repositories name. This filter returns only repositories whose name contains the specified string.
    nameContains :: Core.Maybe Types.NameContains,
    -- | If the result of a @ListCodeRepositoriesOutput@ request was truncated, the response includes a @NextToken@ . To get the next set of Git repositories, use the token in the next request.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The field to sort results by. The default is @Name@ .
    sortBy :: Core.Maybe Types.CodeRepositorySortBy,
    -- | The sort order for results. The default is @Ascending@ .
    sortOrder :: Core.Maybe Types.CodeRepositorySortOrder
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListCodeRepositories' value with any optional fields omitted.
mkListCodeRepositories ::
  ListCodeRepositories
mkListCodeRepositories =
  ListCodeRepositories'
    { creationTimeAfter = Core.Nothing,
      creationTimeBefore = Core.Nothing,
      lastModifiedTimeAfter = Core.Nothing,
      lastModifiedTimeBefore = Core.Nothing,
      maxResults = Core.Nothing,
      nameContains = Core.Nothing,
      nextToken = Core.Nothing,
      sortBy = Core.Nothing,
      sortOrder = Core.Nothing
    }

-- | A filter that returns only Git repositories that were created after the specified time.
--
-- /Note:/ Consider using 'creationTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrCreationTimeAfter :: Lens.Lens' ListCodeRepositories (Core.Maybe Core.NominalDiffTime)
lcrCreationTimeAfter = Lens.field @"creationTimeAfter"
{-# DEPRECATED lcrCreationTimeAfter "Use generic-lens or generic-optics with 'creationTimeAfter' instead." #-}

-- | A filter that returns only Git repositories that were created before the specified time.
--
-- /Note:/ Consider using 'creationTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrCreationTimeBefore :: Lens.Lens' ListCodeRepositories (Core.Maybe Core.NominalDiffTime)
lcrCreationTimeBefore = Lens.field @"creationTimeBefore"
{-# DEPRECATED lcrCreationTimeBefore "Use generic-lens or generic-optics with 'creationTimeBefore' instead." #-}

-- | A filter that returns only Git repositories that were last modified after the specified time.
--
-- /Note:/ Consider using 'lastModifiedTimeAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrLastModifiedTimeAfter :: Lens.Lens' ListCodeRepositories (Core.Maybe Core.NominalDiffTime)
lcrLastModifiedTimeAfter = Lens.field @"lastModifiedTimeAfter"
{-# DEPRECATED lcrLastModifiedTimeAfter "Use generic-lens or generic-optics with 'lastModifiedTimeAfter' instead." #-}

-- | A filter that returns only Git repositories that were last modified before the specified time.
--
-- /Note:/ Consider using 'lastModifiedTimeBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrLastModifiedTimeBefore :: Lens.Lens' ListCodeRepositories (Core.Maybe Core.NominalDiffTime)
lcrLastModifiedTimeBefore = Lens.field @"lastModifiedTimeBefore"
{-# DEPRECATED lcrLastModifiedTimeBefore "Use generic-lens or generic-optics with 'lastModifiedTimeBefore' instead." #-}

-- | The maximum number of Git repositories to return in the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrMaxResults :: Lens.Lens' ListCodeRepositories (Core.Maybe Core.Natural)
lcrMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lcrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A string in the Git repositories name. This filter returns only repositories whose name contains the specified string.
--
-- /Note:/ Consider using 'nameContains' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrNameContains :: Lens.Lens' ListCodeRepositories (Core.Maybe Types.NameContains)
lcrNameContains = Lens.field @"nameContains"
{-# DEPRECATED lcrNameContains "Use generic-lens or generic-optics with 'nameContains' instead." #-}

-- | If the result of a @ListCodeRepositoriesOutput@ request was truncated, the response includes a @NextToken@ . To get the next set of Git repositories, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrNextToken :: Lens.Lens' ListCodeRepositories (Core.Maybe Types.NextToken)
lcrNextToken = Lens.field @"nextToken"
{-# DEPRECATED lcrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The field to sort results by. The default is @Name@ .
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrSortBy :: Lens.Lens' ListCodeRepositories (Core.Maybe Types.CodeRepositorySortBy)
lcrSortBy = Lens.field @"sortBy"
{-# DEPRECATED lcrSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

-- | The sort order for results. The default is @Ascending@ .
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrSortOrder :: Lens.Lens' ListCodeRepositories (Core.Maybe Types.CodeRepositorySortOrder)
lcrSortOrder = Lens.field @"sortOrder"
{-# DEPRECATED lcrSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

instance Core.FromJSON ListCodeRepositories where
  toJSON ListCodeRepositories {..} =
    Core.object
      ( Core.catMaybes
          [ ("CreationTimeAfter" Core..=) Core.<$> creationTimeAfter,
            ("CreationTimeBefore" Core..=) Core.<$> creationTimeBefore,
            ("LastModifiedTimeAfter" Core..=) Core.<$> lastModifiedTimeAfter,
            ("LastModifiedTimeBefore" Core..=) Core.<$> lastModifiedTimeBefore,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NameContains" Core..=) Core.<$> nameContains,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("SortBy" Core..=) Core.<$> sortBy,
            ("SortOrder" Core..=) Core.<$> sortOrder
          ]
      )

instance Core.AWSRequest ListCodeRepositories where
  type Rs ListCodeRepositories = ListCodeRepositoriesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.ListCodeRepositories")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCodeRepositoriesResponse'
            Core.<$> (x Core..:? "CodeRepositorySummaryList" Core..!= Core.mempty)
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListCodeRepositories where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^. Lens.field @"codeRepositorySummaryList") =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListCodeRepositoriesResponse' smart constructor.
data ListCodeRepositoriesResponse = ListCodeRepositoriesResponse'
  { -- | Gets a list of summaries of the Git repositories. Each summary specifies the following values for the repository:
    --
    --
    --     * Name
    --
    --
    --     * Amazon Resource Name (ARN)
    --
    --
    --     * Creation time
    --
    --
    --     * Last modified time
    --
    --
    --     * Configuration information, including the URL location of the repository and the ARN of the AWS Secrets Manager secret that contains the credentials used to access the repository.
    codeRepositorySummaryList :: [Types.CodeRepositorySummary],
    -- | If the result of a @ListCodeRepositoriesOutput@ request was truncated, the response includes a @NextToken@ . To get the next set of Git repositories, use the token in the next request.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListCodeRepositoriesResponse' value with any optional fields omitted.
mkListCodeRepositoriesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListCodeRepositoriesResponse
mkListCodeRepositoriesResponse responseStatus =
  ListCodeRepositoriesResponse'
    { codeRepositorySummaryList =
        Core.mempty,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Gets a list of summaries of the Git repositories. Each summary specifies the following values for the repository:
--
--
--     * Name
--
--
--     * Amazon Resource Name (ARN)
--
--
--     * Creation time
--
--
--     * Last modified time
--
--
--     * Configuration information, including the URL location of the repository and the ARN of the AWS Secrets Manager secret that contains the credentials used to access the repository.
--
--
--
-- /Note:/ Consider using 'codeRepositorySummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrrsCodeRepositorySummaryList :: Lens.Lens' ListCodeRepositoriesResponse [Types.CodeRepositorySummary]
lcrrrsCodeRepositorySummaryList = Lens.field @"codeRepositorySummaryList"
{-# DEPRECATED lcrrrsCodeRepositorySummaryList "Use generic-lens or generic-optics with 'codeRepositorySummaryList' instead." #-}

-- | If the result of a @ListCodeRepositoriesOutput@ request was truncated, the response includes a @NextToken@ . To get the next set of Git repositories, use the token in the next request.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrrsNextToken :: Lens.Lens' ListCodeRepositoriesResponse (Core.Maybe Types.NextToken)
lcrrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lcrrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrrsResponseStatus :: Lens.Lens' ListCodeRepositoriesResponse Core.Int
lcrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lcrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
