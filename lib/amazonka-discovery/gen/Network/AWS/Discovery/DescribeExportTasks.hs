{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.DescribeExportTasks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve status of one or more export tasks. You can retrieve the status of up to 100 export tasks.
--
-- This operation returns paginated results.
module Network.AWS.Discovery.DescribeExportTasks
    (
    -- * Creating a request
      DescribeExportTasks (..)
    , mkDescribeExportTasks
    -- ** Request lenses
    , detExportIds
    , detFilters
    , detMaxResults
    , detNextToken

    -- * Destructuring the response
    , DescribeExportTasksResponse (..)
    , mkDescribeExportTasksResponse
    -- ** Response lenses
    , detrrsExportsInfo
    , detrrsNextToken
    , detrrsResponseStatus
    ) where

import qualified Network.AWS.Discovery.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeExportTasks' smart constructor.
data DescribeExportTasks = DescribeExportTasks'
  { exportIds :: Core.Maybe [Types.ConfigurationsExportId]
    -- ^ One or more unique identifiers used to query the status of an export request.
  , filters :: Core.Maybe [Types.ExportFilter]
    -- ^ One or more filters.
--
--
--     * @AgentId@ - ID of the agent whose collected data will be exported
--
--
  , maxResults :: Core.Maybe Core.Int
    -- ^ The maximum number of volume results returned by @DescribeExportTasks@ in paginated output. When this parameter is used, @DescribeExportTasks@ only returns @maxResults@ results in a single page along with a @nextToken@ response element.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The @nextToken@ value returned from a previous paginated @DescribeExportTasks@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is null when there are no more results to return.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeExportTasks' value with any optional fields omitted.
mkDescribeExportTasks
    :: DescribeExportTasks
mkDescribeExportTasks
  = DescribeExportTasks'{exportIds = Core.Nothing,
                         filters = Core.Nothing, maxResults = Core.Nothing,
                         nextToken = Core.Nothing}

-- | One or more unique identifiers used to query the status of an export request.
--
-- /Note:/ Consider using 'exportIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detExportIds :: Lens.Lens' DescribeExportTasks (Core.Maybe [Types.ConfigurationsExportId])
detExportIds = Lens.field @"exportIds"
{-# INLINEABLE detExportIds #-}
{-# DEPRECATED exportIds "Use generic-lens or generic-optics with 'exportIds' instead"  #-}

-- | One or more filters.
--
--
--     * @AgentId@ - ID of the agent whose collected data will be exported
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detFilters :: Lens.Lens' DescribeExportTasks (Core.Maybe [Types.ExportFilter])
detFilters = Lens.field @"filters"
{-# INLINEABLE detFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of volume results returned by @DescribeExportTasks@ in paginated output. When this parameter is used, @DescribeExportTasks@ only returns @maxResults@ results in a single page along with a @nextToken@ response element.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detMaxResults :: Lens.Lens' DescribeExportTasks (Core.Maybe Core.Int)
detMaxResults = Lens.field @"maxResults"
{-# INLINEABLE detMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The @nextToken@ value returned from a previous paginated @DescribeExportTasks@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is null when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detNextToken :: Lens.Lens' DescribeExportTasks (Core.Maybe Types.NextToken)
detNextToken = Lens.field @"nextToken"
{-# INLINEABLE detNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeExportTasks where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeExportTasks where
        toHeaders DescribeExportTasks{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSPoseidonService_V2015_11_01.DescribeExportTasks")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeExportTasks where
        toJSON DescribeExportTasks{..}
          = Core.object
              (Core.catMaybes
                 [("exportIds" Core..=) Core.<$> exportIds,
                  ("filters" Core..=) Core.<$> filters,
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeExportTasks where
        type Rs DescribeExportTasks = DescribeExportTasksResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeExportTasksResponse' Core.<$>
                   (x Core..:? "exportsInfo") Core.<*> x Core..:? "nextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeExportTasks where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"exportsInfo" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeExportTasksResponse' smart constructor.
data DescribeExportTasksResponse = DescribeExportTasksResponse'
  { exportsInfo :: Core.Maybe [Types.ExportInfo]
    -- ^ Contains one or more sets of export request details. When the status of a request is @SUCCEEDED@ , the response includes a URL for an Amazon S3 bucket where you can view the data in a CSV file.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The @nextToken@ value to include in a future @DescribeExportTasks@ request. When the results of a @DescribeExportTasks@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is null when there are no more results to return.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeExportTasksResponse' value with any optional fields omitted.
mkDescribeExportTasksResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeExportTasksResponse
mkDescribeExportTasksResponse responseStatus
  = DescribeExportTasksResponse'{exportsInfo = Core.Nothing,
                                 nextToken = Core.Nothing, responseStatus}

-- | Contains one or more sets of export request details. When the status of a request is @SUCCEEDED@ , the response includes a URL for an Amazon S3 bucket where you can view the data in a CSV file.
--
-- /Note:/ Consider using 'exportsInfo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrrsExportsInfo :: Lens.Lens' DescribeExportTasksResponse (Core.Maybe [Types.ExportInfo])
detrrsExportsInfo = Lens.field @"exportsInfo"
{-# INLINEABLE detrrsExportsInfo #-}
{-# DEPRECATED exportsInfo "Use generic-lens or generic-optics with 'exportsInfo' instead"  #-}

-- | The @nextToken@ value to include in a future @DescribeExportTasks@ request. When the results of a @DescribeExportTasks@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is null when there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrrsNextToken :: Lens.Lens' DescribeExportTasksResponse (Core.Maybe Types.NextToken)
detrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE detrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrrsResponseStatus :: Lens.Lens' DescribeExportTasksResponse Core.Int
detrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE detrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
