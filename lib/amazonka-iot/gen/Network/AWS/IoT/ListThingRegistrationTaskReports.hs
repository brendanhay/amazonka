{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListThingRegistrationTaskReports
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Information about the thing registration tasks.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListThingRegistrationTaskReports
    (
    -- * Creating a request
      ListThingRegistrationTaskReports (..)
    , mkListThingRegistrationTaskReports
    -- ** Request lenses
    , ltrtrTaskId
    , ltrtrReportType
    , ltrtrMaxResults
    , ltrtrNextToken

    -- * Destructuring the response
    , ListThingRegistrationTaskReportsResponse (..)
    , mkListThingRegistrationTaskReportsResponse
    -- ** Response lenses
    , ltrtrrrsNextToken
    , ltrtrrrsReportType
    , ltrtrrrsResourceLinks
    , ltrtrrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListThingRegistrationTaskReports' smart constructor.
data ListThingRegistrationTaskReports = ListThingRegistrationTaskReports'
  { taskId :: Types.TaskId
    -- ^ The id of the task.
  , reportType :: Types.ReportType
    -- ^ The type of task report.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return per request.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListThingRegistrationTaskReports' value with any optional fields omitted.
mkListThingRegistrationTaskReports
    :: Types.TaskId -- ^ 'taskId'
    -> Types.ReportType -- ^ 'reportType'
    -> ListThingRegistrationTaskReports
mkListThingRegistrationTaskReports taskId reportType
  = ListThingRegistrationTaskReports'{taskId, reportType,
                                      maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The id of the task.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrtrTaskId :: Lens.Lens' ListThingRegistrationTaskReports Types.TaskId
ltrtrTaskId = Lens.field @"taskId"
{-# INLINEABLE ltrtrTaskId #-}
{-# DEPRECATED taskId "Use generic-lens or generic-optics with 'taskId' instead"  #-}

-- | The type of task report.
--
-- /Note:/ Consider using 'reportType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrtrReportType :: Lens.Lens' ListThingRegistrationTaskReports Types.ReportType
ltrtrReportType = Lens.field @"reportType"
{-# INLINEABLE ltrtrReportType #-}
{-# DEPRECATED reportType "Use generic-lens or generic-optics with 'reportType' instead"  #-}

-- | The maximum number of results to return per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrtrMaxResults :: Lens.Lens' ListThingRegistrationTaskReports (Core.Maybe Core.Natural)
ltrtrMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ltrtrMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrtrNextToken :: Lens.Lens' ListThingRegistrationTaskReports (Core.Maybe Types.NextToken)
ltrtrNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltrtrNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListThingRegistrationTaskReports where
        toQuery ListThingRegistrationTaskReports{..}
          = Core.toQueryPair "reportType" reportType Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken

instance Core.ToHeaders ListThingRegistrationTaskReports where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListThingRegistrationTaskReports where
        type Rs ListThingRegistrationTaskReports =
             ListThingRegistrationTaskReportsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/thing-registration-tasks/" Core.<> Core.toText taskId Core.<>
                             "/reports",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListThingRegistrationTaskReportsResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "reportType" Core.<*>
                     x Core..:? "resourceLinks"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListThingRegistrationTaskReports where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"resourceLinks" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListThingRegistrationTaskReportsResponse' smart constructor.
data ListThingRegistrationTaskReportsResponse = ListThingRegistrationTaskReportsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to use to get the next set of results, or __null__ if there are no additional results.
  , reportType :: Core.Maybe Types.ReportType
    -- ^ The type of task report.
  , resourceLinks :: Core.Maybe [Types.S3FileUrl]
    -- ^ Links to the task resources.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListThingRegistrationTaskReportsResponse' value with any optional fields omitted.
mkListThingRegistrationTaskReportsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListThingRegistrationTaskReportsResponse
mkListThingRegistrationTaskReportsResponse responseStatus
  = ListThingRegistrationTaskReportsResponse'{nextToken =
                                                Core.Nothing,
                                              reportType = Core.Nothing,
                                              resourceLinks = Core.Nothing, responseStatus}

-- | The token to use to get the next set of results, or __null__ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrtrrrsNextToken :: Lens.Lens' ListThingRegistrationTaskReportsResponse (Core.Maybe Types.NextToken)
ltrtrrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltrtrrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The type of task report.
--
-- /Note:/ Consider using 'reportType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrtrrrsReportType :: Lens.Lens' ListThingRegistrationTaskReportsResponse (Core.Maybe Types.ReportType)
ltrtrrrsReportType = Lens.field @"reportType"
{-# INLINEABLE ltrtrrrsReportType #-}
{-# DEPRECATED reportType "Use generic-lens or generic-optics with 'reportType' instead"  #-}

-- | Links to the task resources.
--
-- /Note:/ Consider using 'resourceLinks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrtrrrsResourceLinks :: Lens.Lens' ListThingRegistrationTaskReportsResponse (Core.Maybe [Types.S3FileUrl])
ltrtrrrsResourceLinks = Lens.field @"resourceLinks"
{-# INLINEABLE ltrtrrrsResourceLinks #-}
{-# DEPRECATED resourceLinks "Use generic-lens or generic-optics with 'resourceLinks' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrtrrrsResponseStatus :: Lens.Lens' ListThingRegistrationTaskReportsResponse Core.Int
ltrtrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltrtrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
