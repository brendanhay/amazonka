{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.ListRepositories
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about one or more repositories.
--
-- This operation returns paginated results.
module Network.AWS.CodeCommit.ListRepositories
  ( -- * Creating a request
    ListRepositories (..),
    mkListRepositories,

    -- ** Request lenses
    lrNextToken,
    lrOrder,
    lrSortBy,

    -- * Destructuring the response
    ListRepositoriesResponse (..),
    mkListRepositoriesResponse,

    -- ** Response lenses
    lrrrsNextToken,
    lrrrsRepositories,
    lrrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeCommit.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input of a list repositories operation.
--
-- /See:/ 'mkListRepositories' smart constructor.
data ListRepositories = ListRepositories'
  { -- | An enumeration token that allows the operation to batch the results of the operation. Batch sizes are 1,000 for list repository operations. When the client sends the token back to AWS CodeCommit, another page of 1,000 records is retrieved.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The order in which to sort the results of a list repositories operation.
    order :: Core.Maybe Types.OrderEnum,
    -- | The criteria used to sort the results of a list repositories operation.
    sortBy :: Core.Maybe Types.SortByEnum
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListRepositories' value with any optional fields omitted.
mkListRepositories ::
  ListRepositories
mkListRepositories =
  ListRepositories'
    { nextToken = Core.Nothing,
      order = Core.Nothing,
      sortBy = Core.Nothing
    }

-- | An enumeration token that allows the operation to batch the results of the operation. Batch sizes are 1,000 for list repository operations. When the client sends the token back to AWS CodeCommit, another page of 1,000 records is retrieved.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrNextToken :: Lens.Lens' ListRepositories (Core.Maybe Types.NextToken)
lrNextToken = Lens.field @"nextToken"
{-# DEPRECATED lrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The order in which to sort the results of a list repositories operation.
--
-- /Note:/ Consider using 'order' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrOrder :: Lens.Lens' ListRepositories (Core.Maybe Types.OrderEnum)
lrOrder = Lens.field @"order"
{-# DEPRECATED lrOrder "Use generic-lens or generic-optics with 'order' instead." #-}

-- | The criteria used to sort the results of a list repositories operation.
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrSortBy :: Lens.Lens' ListRepositories (Core.Maybe Types.SortByEnum)
lrSortBy = Lens.field @"sortBy"
{-# DEPRECATED lrSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

instance Core.FromJSON ListRepositories where
  toJSON ListRepositories {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("order" Core..=) Core.<$> order,
            ("sortBy" Core..=) Core.<$> sortBy
          ]
      )

instance Core.AWSRequest ListRepositories where
  type Rs ListRepositories = ListRepositoriesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "CodeCommit_20150413.ListRepositories")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRepositoriesResponse'
            Core.<$> (x Core..:? "nextToken")
            Core.<*> (x Core..:? "repositories")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListRepositories where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"repositories" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | Represents the output of a list repositories operation.
--
-- /See:/ 'mkListRepositoriesResponse' smart constructor.
data ListRepositoriesResponse = ListRepositoriesResponse'
  { -- | An enumeration token that allows the operation to batch the results of the operation. Batch sizes are 1,000 for list repository operations. When the client sends the token back to AWS CodeCommit, another page of 1,000 records is retrieved.
    nextToken :: Core.Maybe Types.NextToken,
    -- | Lists the repositories called by the list repositories operation.
    repositories :: Core.Maybe [Types.RepositoryNameIdPair],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListRepositoriesResponse' value with any optional fields omitted.
mkListRepositoriesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListRepositoriesResponse
mkListRepositoriesResponse responseStatus =
  ListRepositoriesResponse'
    { nextToken = Core.Nothing,
      repositories = Core.Nothing,
      responseStatus
    }

-- | An enumeration token that allows the operation to batch the results of the operation. Batch sizes are 1,000 for list repository operations. When the client sends the token back to AWS CodeCommit, another page of 1,000 records is retrieved.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrrsNextToken :: Lens.Lens' ListRepositoriesResponse (Core.Maybe Types.NextToken)
lrrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lrrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Lists the repositories called by the list repositories operation.
--
-- /Note:/ Consider using 'repositories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrrsRepositories :: Lens.Lens' ListRepositoriesResponse (Core.Maybe [Types.RepositoryNameIdPair])
lrrrsRepositories = Lens.field @"repositories"
{-# DEPRECATED lrrrsRepositories "Use generic-lens or generic-optics with 'repositories' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrrsResponseStatus :: Lens.Lens' ListRepositoriesResponse Core.Int
lrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
