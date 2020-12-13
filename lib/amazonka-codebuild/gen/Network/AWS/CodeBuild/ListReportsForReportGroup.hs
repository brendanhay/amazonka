{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.ListReportsForReportGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of ARNs for the reports that belong to a @ReportGroup@ .
--
-- This operation returns paginated results.
module Network.AWS.CodeBuild.ListReportsForReportGroup
  ( -- * Creating a request
    ListReportsForReportGroup (..),
    mkListReportsForReportGroup,

    -- ** Request lenses
    lrfrgReportGroupARN,
    lrfrgSortOrder,
    lrfrgNextToken,
    lrfrgFilter,
    lrfrgMaxResults,

    -- * Destructuring the response
    ListReportsForReportGroupResponse (..),
    mkListReportsForReportGroupResponse,

    -- ** Response lenses
    lrfrgrsReports,
    lrfrgrsNextToken,
    lrfrgrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListReportsForReportGroup' smart constructor.
data ListReportsForReportGroup = ListReportsForReportGroup'
  { -- | The ARN of the report group for which you want to return report ARNs.
    reportGroupARN :: Lude.Text,
    -- | Use to specify whether the results are returned in ascending or descending order.
    sortOrder :: Lude.Maybe SortOrderType,
    -- | During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
    nextToken :: Lude.Maybe Lude.Text,
    -- | A @ReportFilter@ object used to filter the returned reports.
    filter :: Lude.Maybe ReportFilter,
    -- | The maximum number of paginated reports in this report group returned per response. Use @nextToken@ to iterate pages in the list of returned @Report@ objects. The default value is 100.
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListReportsForReportGroup' with the minimum fields required to make a request.
--
-- * 'reportGroupARN' - The ARN of the report group for which you want to return report ARNs.
-- * 'sortOrder' - Use to specify whether the results are returned in ascending or descending order.
-- * 'nextToken' - During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
-- * 'filter' - A @ReportFilter@ object used to filter the returned reports.
-- * 'maxResults' - The maximum number of paginated reports in this report group returned per response. Use @nextToken@ to iterate pages in the list of returned @Report@ objects. The default value is 100.
mkListReportsForReportGroup ::
  -- | 'reportGroupARN'
  Lude.Text ->
  ListReportsForReportGroup
mkListReportsForReportGroup pReportGroupARN_ =
  ListReportsForReportGroup'
    { reportGroupARN = pReportGroupARN_,
      sortOrder = Lude.Nothing,
      nextToken = Lude.Nothing,
      filter = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | The ARN of the report group for which you want to return report ARNs.
--
-- /Note:/ Consider using 'reportGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrfrgReportGroupARN :: Lens.Lens' ListReportsForReportGroup Lude.Text
lrfrgReportGroupARN = Lens.lens (reportGroupARN :: ListReportsForReportGroup -> Lude.Text) (\s a -> s {reportGroupARN = a} :: ListReportsForReportGroup)
{-# DEPRECATED lrfrgReportGroupARN "Use generic-lens or generic-optics with 'reportGroupARN' instead." #-}

-- | Use to specify whether the results are returned in ascending or descending order.
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrfrgSortOrder :: Lens.Lens' ListReportsForReportGroup (Lude.Maybe SortOrderType)
lrfrgSortOrder = Lens.lens (sortOrder :: ListReportsForReportGroup -> Lude.Maybe SortOrderType) (\s a -> s {sortOrder = a} :: ListReportsForReportGroup)
{-# DEPRECATED lrfrgSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrfrgNextToken :: Lens.Lens' ListReportsForReportGroup (Lude.Maybe Lude.Text)
lrfrgNextToken = Lens.lens (nextToken :: ListReportsForReportGroup -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListReportsForReportGroup)
{-# DEPRECATED lrfrgNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A @ReportFilter@ object used to filter the returned reports.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrfrgFilter :: Lens.Lens' ListReportsForReportGroup (Lude.Maybe ReportFilter)
lrfrgFilter = Lens.lens (filter :: ListReportsForReportGroup -> Lude.Maybe ReportFilter) (\s a -> s {filter = a} :: ListReportsForReportGroup)
{-# DEPRECATED lrfrgFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The maximum number of paginated reports in this report group returned per response. Use @nextToken@ to iterate pages in the list of returned @Report@ objects. The default value is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrfrgMaxResults :: Lens.Lens' ListReportsForReportGroup (Lude.Maybe Lude.Natural)
lrfrgMaxResults = Lens.lens (maxResults :: ListReportsForReportGroup -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListReportsForReportGroup)
{-# DEPRECATED lrfrgMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListReportsForReportGroup where
  page rq rs
    | Page.stop (rs Lens.^. lrfrgrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lrfrgrsReports) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lrfrgNextToken Lens..~ rs Lens.^. lrfrgrsNextToken

instance Lude.AWSRequest ListReportsForReportGroup where
  type
    Rs ListReportsForReportGroup =
      ListReportsForReportGroupResponse
  request = Req.postJSON codeBuildService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListReportsForReportGroupResponse'
            Lude.<$> (x Lude..?> "reports")
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListReportsForReportGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "CodeBuild_20161006.ListReportsForReportGroup" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListReportsForReportGroup where
  toJSON ListReportsForReportGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("reportGroupArn" Lude..= reportGroupARN),
            ("sortOrder" Lude..=) Lude.<$> sortOrder,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("filter" Lude..=) Lude.<$> filter,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListReportsForReportGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery ListReportsForReportGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListReportsForReportGroupResponse' smart constructor.
data ListReportsForReportGroupResponse = ListReportsForReportGroupResponse'
  { -- | The list of report ARNs.
    reports :: Lude.Maybe (Lude.NonEmpty Lude.Text),
    -- | During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
    nextToken :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListReportsForReportGroupResponse' with the minimum fields required to make a request.
--
-- * 'reports' - The list of report ARNs.
-- * 'nextToken' - During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
-- * 'responseStatus' - The response status code.
mkListReportsForReportGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListReportsForReportGroupResponse
mkListReportsForReportGroupResponse pResponseStatus_ =
  ListReportsForReportGroupResponse'
    { reports = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of report ARNs.
--
-- /Note:/ Consider using 'reports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrfrgrsReports :: Lens.Lens' ListReportsForReportGroupResponse (Lude.Maybe (Lude.NonEmpty Lude.Text))
lrfrgrsReports = Lens.lens (reports :: ListReportsForReportGroupResponse -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {reports = a} :: ListReportsForReportGroupResponse)
{-# DEPRECATED lrfrgrsReports "Use generic-lens or generic-optics with 'reports' instead." #-}

-- | During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrfrgrsNextToken :: Lens.Lens' ListReportsForReportGroupResponse (Lude.Maybe Lude.Text)
lrfrgrsNextToken = Lens.lens (nextToken :: ListReportsForReportGroupResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListReportsForReportGroupResponse)
{-# DEPRECATED lrfrgrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrfrgrsResponseStatus :: Lens.Lens' ListReportsForReportGroupResponse Lude.Int
lrfrgrsResponseStatus = Lens.lens (responseStatus :: ListReportsForReportGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListReportsForReportGroupResponse)
{-# DEPRECATED lrfrgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
