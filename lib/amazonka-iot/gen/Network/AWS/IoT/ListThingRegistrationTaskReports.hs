{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    ltrtrNextToken,
    ltrtrMaxResults,
    ltrtrTaskId,
    ltrtrReportType,

    -- * Destructuring the response
    ListThingRegistrationTaskReportsResponse (..),
    mkListThingRegistrationTaskReportsResponse,

    -- ** Response lenses
    ltrtrrsResourceLinks,
    ltrtrrsNextToken,
    ltrtrrsReportType,
    ltrtrrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListThingRegistrationTaskReports' smart constructor.
data ListThingRegistrationTaskReports = ListThingRegistrationTaskReports'
  { nextToken ::
      Lude.Maybe Lude.Text,
    maxResults ::
      Lude.Maybe Lude.Natural,
    taskId :: Lude.Text,
    reportType :: ReportType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListThingRegistrationTaskReports' with the minimum fields required to make a request.
--
-- * 'maxResults' - The maximum number of results to return per request.
-- * 'nextToken' - To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
-- * 'reportType' - The type of task report.
-- * 'taskId' - The id of the task.
mkListThingRegistrationTaskReports ::
  -- | 'taskId'
  Lude.Text ->
  -- | 'reportType'
  ReportType ->
  ListThingRegistrationTaskReports
mkListThingRegistrationTaskReports pTaskId_ pReportType_ =
  ListThingRegistrationTaskReports'
    { nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      taskId = pTaskId_,
      reportType = pReportType_
    }

-- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrtrNextToken :: Lens.Lens' ListThingRegistrationTaskReports (Lude.Maybe Lude.Text)
ltrtrNextToken = Lens.lens (nextToken :: ListThingRegistrationTaskReports -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListThingRegistrationTaskReports)
{-# DEPRECATED ltrtrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrtrMaxResults :: Lens.Lens' ListThingRegistrationTaskReports (Lude.Maybe Lude.Natural)
ltrtrMaxResults = Lens.lens (maxResults :: ListThingRegistrationTaskReports -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListThingRegistrationTaskReports)
{-# DEPRECATED ltrtrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The id of the task.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrtrTaskId :: Lens.Lens' ListThingRegistrationTaskReports Lude.Text
ltrtrTaskId = Lens.lens (taskId :: ListThingRegistrationTaskReports -> Lude.Text) (\s a -> s {taskId = a} :: ListThingRegistrationTaskReports)
{-# DEPRECATED ltrtrTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

-- | The type of task report.
--
-- /Note:/ Consider using 'reportType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrtrReportType :: Lens.Lens' ListThingRegistrationTaskReports ReportType
ltrtrReportType = Lens.lens (reportType :: ListThingRegistrationTaskReports -> ReportType) (\s a -> s {reportType = a} :: ListThingRegistrationTaskReports)
{-# DEPRECATED ltrtrReportType "Use generic-lens or generic-optics with 'reportType' instead." #-}

instance Page.AWSPager ListThingRegistrationTaskReports where
  page rq rs
    | Page.stop (rs Lens.^. ltrtrrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. ltrtrrsResourceLinks) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& ltrtrNextToken Lens..~ rs Lens.^. ltrtrrsNextToken

instance Lude.AWSRequest ListThingRegistrationTaskReports where
  type
    Rs ListThingRegistrationTaskReports =
      ListThingRegistrationTaskReportsResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListThingRegistrationTaskReportsResponse'
            Lude.<$> (x Lude..?> "resourceLinks" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "reportType")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListThingRegistrationTaskReports where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListThingRegistrationTaskReports where
  toPath ListThingRegistrationTaskReports' {..} =
    Lude.mconcat
      ["/thing-registration-tasks/", Lude.toBS taskId, "/reports"]

instance Lude.ToQuery ListThingRegistrationTaskReports where
  toQuery ListThingRegistrationTaskReports' {..} =
    Lude.mconcat
      [ "nextToken" Lude.=: nextToken,
        "maxResults" Lude.=: maxResults,
        "reportType" Lude.=: reportType
      ]

-- | /See:/ 'mkListThingRegistrationTaskReportsResponse' smart constructor.
data ListThingRegistrationTaskReportsResponse = ListThingRegistrationTaskReportsResponse'
  { resourceLinks ::
      Lude.Maybe
        [Lude.Text],
    nextToken ::
      Lude.Maybe
        Lude.Text,
    reportType ::
      Lude.Maybe
        ReportType,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListThingRegistrationTaskReportsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to use to get the next set of results, or __null__ if there are no additional results.
-- * 'reportType' - The type of task report.
-- * 'resourceLinks' - Links to the task resources.
-- * 'responseStatus' - The response status code.
mkListThingRegistrationTaskReportsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListThingRegistrationTaskReportsResponse
mkListThingRegistrationTaskReportsResponse pResponseStatus_ =
  ListThingRegistrationTaskReportsResponse'
    { resourceLinks =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      reportType = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Links to the task resources.
--
-- /Note:/ Consider using 'resourceLinks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrtrrsResourceLinks :: Lens.Lens' ListThingRegistrationTaskReportsResponse (Lude.Maybe [Lude.Text])
ltrtrrsResourceLinks = Lens.lens (resourceLinks :: ListThingRegistrationTaskReportsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {resourceLinks = a} :: ListThingRegistrationTaskReportsResponse)
{-# DEPRECATED ltrtrrsResourceLinks "Use generic-lens or generic-optics with 'resourceLinks' instead." #-}

-- | The token to use to get the next set of results, or __null__ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrtrrsNextToken :: Lens.Lens' ListThingRegistrationTaskReportsResponse (Lude.Maybe Lude.Text)
ltrtrrsNextToken = Lens.lens (nextToken :: ListThingRegistrationTaskReportsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListThingRegistrationTaskReportsResponse)
{-# DEPRECATED ltrtrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The type of task report.
--
-- /Note:/ Consider using 'reportType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrtrrsReportType :: Lens.Lens' ListThingRegistrationTaskReportsResponse (Lude.Maybe ReportType)
ltrtrrsReportType = Lens.lens (reportType :: ListThingRegistrationTaskReportsResponse -> Lude.Maybe ReportType) (\s a -> s {reportType = a} :: ListThingRegistrationTaskReportsResponse)
{-# DEPRECATED ltrtrrsReportType "Use generic-lens or generic-optics with 'reportType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltrtrrsResponseStatus :: Lens.Lens' ListThingRegistrationTaskReportsResponse Lude.Int
ltrtrrsResponseStatus = Lens.lens (responseStatus :: ListThingRegistrationTaskReportsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListThingRegistrationTaskReportsResponse)
{-# DEPRECATED ltrtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
