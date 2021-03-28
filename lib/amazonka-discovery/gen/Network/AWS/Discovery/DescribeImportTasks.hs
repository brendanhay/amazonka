{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.DescribeImportTasks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of import tasks for your account, including status information, times, IDs, the Amazon S3 Object URL for the import file, and more.
module Network.AWS.Discovery.DescribeImportTasks
    (
    -- * Creating a request
      DescribeImportTasks (..)
    , mkDescribeImportTasks
    -- ** Request lenses
    , ditFilters
    , ditMaxResults
    , ditNextToken

    -- * Destructuring the response
    , DescribeImportTasksResponse (..)
    , mkDescribeImportTasksResponse
    -- ** Response lenses
    , ditrrsNextToken
    , ditrrsTasks
    , ditrrsResponseStatus
    ) where

import qualified Network.AWS.Discovery.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeImportTasks' smart constructor.
data DescribeImportTasks = DescribeImportTasks'
  { filters :: Core.Maybe [Types.ImportTaskFilter]
    -- ^ An array of name-value pairs that you provide to filter the results for the @DescribeImportTask@ request to a specific subset of results. Currently, wildcard values aren't supported for filters.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results that you want this request to return, up to 100.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to request a specific page of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeImportTasks' value with any optional fields omitted.
mkDescribeImportTasks
    :: DescribeImportTasks
mkDescribeImportTasks
  = DescribeImportTasks'{filters = Core.Nothing,
                         maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | An array of name-value pairs that you provide to filter the results for the @DescribeImportTask@ request to a specific subset of results. Currently, wildcard values aren't supported for filters.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditFilters :: Lens.Lens' DescribeImportTasks (Core.Maybe [Types.ImportTaskFilter])
ditFilters = Lens.field @"filters"
{-# INLINEABLE ditFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of results that you want this request to return, up to 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditMaxResults :: Lens.Lens' DescribeImportTasks (Core.Maybe Core.Natural)
ditMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ditMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token to request a specific page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditNextToken :: Lens.Lens' DescribeImportTasks (Core.Maybe Types.NextToken)
ditNextToken = Lens.field @"nextToken"
{-# INLINEABLE ditNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeImportTasks where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeImportTasks where
        toHeaders DescribeImportTasks{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSPoseidonService_V2015_11_01.DescribeImportTasks")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeImportTasks where
        toJSON DescribeImportTasks{..}
          = Core.object
              (Core.catMaybes
                 [("filters" Core..=) Core.<$> filters,
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeImportTasks where
        type Rs DescribeImportTasks = DescribeImportTasksResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeImportTasksResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "tasks" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeImportTasksResponse' smart constructor.
data DescribeImportTasksResponse = DescribeImportTasksResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to request the next page of results.
  , tasks :: Core.Maybe [Types.ImportTask]
    -- ^ A returned array of import tasks that match any applied filters, up to the specified number of maximum results.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeImportTasksResponse' value with any optional fields omitted.
mkDescribeImportTasksResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeImportTasksResponse
mkDescribeImportTasksResponse responseStatus
  = DescribeImportTasksResponse'{nextToken = Core.Nothing,
                                 tasks = Core.Nothing, responseStatus}

-- | The token to request the next page of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditrrsNextToken :: Lens.Lens' DescribeImportTasksResponse (Core.Maybe Types.NextToken)
ditrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ditrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A returned array of import tasks that match any applied filters, up to the specified number of maximum results.
--
-- /Note:/ Consider using 'tasks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditrrsTasks :: Lens.Lens' DescribeImportTasksResponse (Core.Maybe [Types.ImportTask])
ditrrsTasks = Lens.field @"tasks"
{-# INLINEABLE ditrrsTasks #-}
{-# DEPRECATED tasks "Use generic-lens or generic-optics with 'tasks' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ditrrsResponseStatus :: Lens.Lens' DescribeImportTasksResponse Core.Int
ditrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ditrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
