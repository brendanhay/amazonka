{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ListThingRegistrationTaskReports (..),
    mkListThingRegistrationTaskReports,

    -- ** Request lenses
    ltrtrTaskId,
    ltrtrReportType,
    ltrtrMaxResults,
    ltrtrNextToken,

    -- * Destructuring the response
    ListThingRegistrationTaskReportsResponse (..),
    mkListThingRegistrationTaskReportsResponse,

    -- ** Response lenses
    ltrtrrrsNextToken,
    ltrtrrrsReportType,
    ltrtrrrsResourceLinks,
    ltrtrrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListThingRegistrationTaskReports' smart constructor.
data ListThingRegistrationTaskReports = ListThingRegistrationTaskReports'
  { -- | The id of the task.
    taskId :: Types.TaskId,
    -- | The type of task report.
    reportType :: Types.ReportType,
    -- | The maximum number of results to return per request.
    maxResults :: Core.Maybe Core.Natural,
    -- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListThingRegistrationTaskReports' value with any optional fields omitted.
mkListThingRegistrationTaskReports ::
  -- | 'taskId'
  Types.TaskId ->
  -- | 'reportType'
  Types.ReportType ->
  ListThingRegistrationTaskReports
mkListThingRegistrationTaskReports taskId reportType =
  ListThingRegistrationTaskReports'
    { taskId,
      reportType,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The id of the task.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrtrTaskId :: Lens.Lens' ListThingRegistrationTaskReports Types.TaskId
ltrtrTaskId = Lens.field @"taskId"
{-# DEPRECATED ltrtrTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

-- | The type of task report.
--
-- /Note:/ Consider using 'reportType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrtrReportType :: Lens.Lens' ListThingRegistrationTaskReports Types.ReportType
ltrtrReportType = Lens.field @"reportType"
{-# DEPRECATED ltrtrReportType "Use generic-lens or generic-optics with 'reportType' instead." #-}

-- | The maximum number of results to return per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrtrMaxResults :: Lens.Lens' ListThingRegistrationTaskReports (Core.Maybe Core.Natural)
ltrtrMaxResults = Lens.field @"maxResults"
{-# DEPRECATED ltrtrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrtrNextToken :: Lens.Lens' ListThingRegistrationTaskReports (Core.Maybe Types.NextToken)
ltrtrNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltrtrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListThingRegistrationTaskReports where
  type
    Rs ListThingRegistrationTaskReports =
      ListThingRegistrationTaskReportsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/thing-registration-tasks/" Core.<> (Core.toText taskId)
                Core.<> ("/reports")
            ),
        Core._rqQuery =
          Core.toQueryValue "reportType" reportType
            Core.<> (Core.toQueryValue "maxResults" Core.<$> maxResults)
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListThingRegistrationTaskReportsResponse'
            Core.<$> (x Core..:? "nextToken")
            Core.<*> (x Core..:? "reportType")
            Core.<*> (x Core..:? "resourceLinks")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListThingRegistrationTaskReports where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"resourceLinks" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListThingRegistrationTaskReportsResponse' smart constructor.
data ListThingRegistrationTaskReportsResponse = ListThingRegistrationTaskReportsResponse'
  { -- | The token to use to get the next set of results, or __null__ if there are no additional results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The type of task report.
    reportType :: Core.Maybe Types.ReportType,
    -- | Links to the task resources.
    resourceLinks :: Core.Maybe [Types.S3FileUrl],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListThingRegistrationTaskReportsResponse' value with any optional fields omitted.
mkListThingRegistrationTaskReportsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListThingRegistrationTaskReportsResponse
mkListThingRegistrationTaskReportsResponse responseStatus =
  ListThingRegistrationTaskReportsResponse'
    { nextToken =
        Core.Nothing,
      reportType = Core.Nothing,
      resourceLinks = Core.Nothing,
      responseStatus
    }

-- | The token to use to get the next set of results, or __null__ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrtrrrsNextToken :: Lens.Lens' ListThingRegistrationTaskReportsResponse (Core.Maybe Types.NextToken)
ltrtrrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltrtrrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The type of task report.
--
-- /Note:/ Consider using 'reportType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrtrrrsReportType :: Lens.Lens' ListThingRegistrationTaskReportsResponse (Core.Maybe Types.ReportType)
ltrtrrrsReportType = Lens.field @"reportType"
{-# DEPRECATED ltrtrrrsReportType "Use generic-lens or generic-optics with 'reportType' instead." #-}

-- | Links to the task resources.
--
-- /Note:/ Consider using 'resourceLinks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrtrrrsResourceLinks :: Lens.Lens' ListThingRegistrationTaskReportsResponse (Core.Maybe [Types.S3FileUrl])
ltrtrrrsResourceLinks = Lens.field @"resourceLinks"
{-# DEPRECATED ltrtrrrsResourceLinks "Use generic-lens or generic-optics with 'resourceLinks' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrtrrrsResponseStatus :: Lens.Lens' ListThingRegistrationTaskReportsResponse Core.Int
ltrtrrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ltrtrrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
