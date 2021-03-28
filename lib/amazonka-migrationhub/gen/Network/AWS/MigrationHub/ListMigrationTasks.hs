{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.ListMigrationTasks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all, or filtered by resource name, migration tasks associated with the user account making this call. This API has the following traits:
--
--
--     * Can show a summary list of the most recent migration tasks.
--
--
--     * Can show a summary list of migration tasks associated with a given discovered resource.
--
--
--     * Lists migration tasks in a paginated interface.
--
--
--
-- This operation returns paginated results.
module Network.AWS.MigrationHub.ListMigrationTasks
    (
    -- * Creating a request
      ListMigrationTasks (..)
    , mkListMigrationTasks
    -- ** Request lenses
    , lmtMaxResults
    , lmtNextToken
    , lmtResourceName

    -- * Destructuring the response
    , ListMigrationTasksResponse (..)
    , mkListMigrationTasksResponse
    -- ** Response lenses
    , lmtrrsMigrationTaskSummaryList
    , lmtrrsNextToken
    , lmtrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MigrationHub.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListMigrationTasks' smart constructor.
data ListMigrationTasks = ListMigrationTasks'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ Value to specify how many results are returned per page.
  , nextToken :: Core.Maybe Types.Token
    -- ^ If a @NextToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @NextToken@ .
  , resourceName :: Core.Maybe Types.ResourceName
    -- ^ Filter migration tasks by discovered resource name.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListMigrationTasks' value with any optional fields omitted.
mkListMigrationTasks
    :: ListMigrationTasks
mkListMigrationTasks
  = ListMigrationTasks'{maxResults = Core.Nothing,
                        nextToken = Core.Nothing, resourceName = Core.Nothing}

-- | Value to specify how many results are returned per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmtMaxResults :: Lens.Lens' ListMigrationTasks (Core.Maybe Core.Natural)
lmtMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lmtMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | If a @NextToken@ was returned by a previous call, there are more results available. To retrieve the next page of results, make the call again using the returned token in @NextToken@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmtNextToken :: Lens.Lens' ListMigrationTasks (Core.Maybe Types.Token)
lmtNextToken = Lens.field @"nextToken"
{-# INLINEABLE lmtNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Filter migration tasks by discovered resource name.
--
-- /Note:/ Consider using 'resourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmtResourceName :: Lens.Lens' ListMigrationTasks (Core.Maybe Types.ResourceName)
lmtResourceName = Lens.field @"resourceName"
{-# INLINEABLE lmtResourceName #-}
{-# DEPRECATED resourceName "Use generic-lens or generic-optics with 'resourceName' instead"  #-}

instance Core.ToQuery ListMigrationTasks where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListMigrationTasks where
        toHeaders ListMigrationTasks{..}
          = Core.pure ("X-Amz-Target", "AWSMigrationHub.ListMigrationTasks")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListMigrationTasks where
        toJSON ListMigrationTasks{..}
          = Core.object
              (Core.catMaybes
                 [("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("ResourceName" Core..=) Core.<$> resourceName])

instance Core.AWSRequest ListMigrationTasks where
        type Rs ListMigrationTasks = ListMigrationTasksResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListMigrationTasksResponse' Core.<$>
                   (x Core..:? "MigrationTaskSummaryList") Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListMigrationTasks where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^?
                 Lens.field @"migrationTaskSummaryList" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListMigrationTasksResponse' smart constructor.
data ListMigrationTasksResponse = ListMigrationTasksResponse'
  { migrationTaskSummaryList :: Core.Maybe [Types.MigrationTaskSummary]
    -- ^ Lists the migration task's summary which includes: @MigrationTaskName@ , @ProgressPercent@ , @ProgressUpdateStream@ , @Status@ , and the @UpdateDateTime@ for each task.
  , nextToken :: Core.Maybe Types.Token
    -- ^ If there are more migration tasks than the max result, return the next token to be passed to the next call as a bookmark of where to start from.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ListMigrationTasksResponse' value with any optional fields omitted.
mkListMigrationTasksResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListMigrationTasksResponse
mkListMigrationTasksResponse responseStatus
  = ListMigrationTasksResponse'{migrationTaskSummaryList =
                                  Core.Nothing,
                                nextToken = Core.Nothing, responseStatus}

-- | Lists the migration task's summary which includes: @MigrationTaskName@ , @ProgressPercent@ , @ProgressUpdateStream@ , @Status@ , and the @UpdateDateTime@ for each task.
--
-- /Note:/ Consider using 'migrationTaskSummaryList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmtrrsMigrationTaskSummaryList :: Lens.Lens' ListMigrationTasksResponse (Core.Maybe [Types.MigrationTaskSummary])
lmtrrsMigrationTaskSummaryList = Lens.field @"migrationTaskSummaryList"
{-# INLINEABLE lmtrrsMigrationTaskSummaryList #-}
{-# DEPRECATED migrationTaskSummaryList "Use generic-lens or generic-optics with 'migrationTaskSummaryList' instead"  #-}

-- | If there are more migration tasks than the max result, return the next token to be passed to the next call as a bookmark of where to start from.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmtrrsNextToken :: Lens.Lens' ListMigrationTasksResponse (Core.Maybe Types.Token)
lmtrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lmtrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lmtrrsResponseStatus :: Lens.Lens' ListMigrationTasksResponse Core.Int
lmtrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lmtrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
