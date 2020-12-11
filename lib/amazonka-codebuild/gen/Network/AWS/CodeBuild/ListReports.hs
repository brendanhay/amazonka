{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.ListReports
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of ARNs for the reports in the current AWS account.
--
-- This operation returns paginated results.
module Network.AWS.CodeBuild.ListReports
  ( -- * Creating a request
    ListReports (..),
    mkListReports,

    -- ** Request lenses
    lrSortOrder,
    lrNextToken,
    lrFilter,
    lrMaxResults,

    -- * Destructuring the response
    ListReportsResponse (..),
    mkListReportsResponse,

    -- ** Response lenses
    lrrsReports,
    lrrsNextToken,
    lrrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListReports' smart constructor.
data ListReports = ListReports'
  { sortOrder ::
      Lude.Maybe SortOrderType,
    nextToken :: Lude.Maybe Lude.Text,
    filter :: Lude.Maybe ReportFilter,
    maxResults :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListReports' with the minimum fields required to make a request.
--
-- * 'filter' - A @ReportFilter@ object used to filter the returned reports.
-- * 'maxResults' - The maximum number of paginated reports returned per response. Use @nextToken@ to iterate pages in the list of returned @Report@ objects. The default value is 100.
-- * 'nextToken' - During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
-- * 'sortOrder' - Specifies the sort order for the list of returned reports. Valid values are:
--
--
--     * @ASCENDING@ : return reports in chronological order based on their creation date.
--
--
--     * @DESCENDING@ : return reports in the reverse chronological order based on their creation date.
mkListReports ::
  ListReports
mkListReports =
  ListReports'
    { sortOrder = Lude.Nothing,
      nextToken = Lude.Nothing,
      filter = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | Specifies the sort order for the list of returned reports. Valid values are:
--
--
--     * @ASCENDING@ : return reports in chronological order based on their creation date.
--
--
--     * @DESCENDING@ : return reports in the reverse chronological order based on their creation date.
--
--
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrSortOrder :: Lens.Lens' ListReports (Lude.Maybe SortOrderType)
lrSortOrder = Lens.lens (sortOrder :: ListReports -> Lude.Maybe SortOrderType) (\s a -> s {sortOrder = a} :: ListReports)
{-# DEPRECATED lrSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrNextToken :: Lens.Lens' ListReports (Lude.Maybe Lude.Text)
lrNextToken = Lens.lens (nextToken :: ListReports -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListReports)
{-# DEPRECATED lrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A @ReportFilter@ object used to filter the returned reports.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrFilter :: Lens.Lens' ListReports (Lude.Maybe ReportFilter)
lrFilter = Lens.lens (filter :: ListReports -> Lude.Maybe ReportFilter) (\s a -> s {filter = a} :: ListReports)
{-# DEPRECATED lrFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The maximum number of paginated reports returned per response. Use @nextToken@ to iterate pages in the list of returned @Report@ objects. The default value is 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrMaxResults :: Lens.Lens' ListReports (Lude.Maybe Lude.Natural)
lrMaxResults = Lens.lens (maxResults :: ListReports -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: ListReports)
{-# DEPRECATED lrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance Page.AWSPager ListReports where
  page rq rs
    | Page.stop (rs Lens.^. lrrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. lrrsReports) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& lrNextToken Lens..~ rs Lens.^. lrrsNextToken

instance Lude.AWSRequest ListReports where
  type Rs ListReports = ListReportsResponse
  request = Req.postJSON codeBuildService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListReportsResponse'
            Lude.<$> (x Lude..?> "reports")
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListReports where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeBuild_20161006.ListReports" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListReports where
  toJSON ListReports' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("sortOrder" Lude..=) Lude.<$> sortOrder,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("filter" Lude..=) Lude.<$> filter,
            ("maxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath ListReports where
  toPath = Lude.const "/"

instance Lude.ToQuery ListReports where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListReportsResponse' smart constructor.
data ListReportsResponse = ListReportsResponse'
  { reports ::
      Lude.Maybe (Lude.NonEmpty Lude.Text),
    nextToken :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListReportsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
-- * 'reports' - The list of returned ARNs for the reports in the current AWS account.
-- * 'responseStatus' - The response status code.
mkListReportsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListReportsResponse
mkListReportsResponse pResponseStatus_ =
  ListReportsResponse'
    { reports = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of returned ARNs for the reports in the current AWS account.
--
-- /Note:/ Consider using 'reports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsReports :: Lens.Lens' ListReportsResponse (Lude.Maybe (Lude.NonEmpty Lude.Text))
lrrsReports = Lens.lens (reports :: ListReportsResponse -> Lude.Maybe (Lude.NonEmpty Lude.Text)) (\s a -> s {reports = a} :: ListReportsResponse)
{-# DEPRECATED lrrsReports "Use generic-lens or generic-optics with 'reports' instead." #-}

-- | During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsNextToken :: Lens.Lens' ListReportsResponse (Lude.Maybe Lude.Text)
lrrsNextToken = Lens.lens (nextToken :: ListReportsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: ListReportsResponse)
{-# DEPRECATED lrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrsResponseStatus :: Lens.Lens' ListReportsResponse Lude.Int
lrrsResponseStatus = Lens.lens (responseStatus :: ListReportsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListReportsResponse)
{-# DEPRECATED lrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
