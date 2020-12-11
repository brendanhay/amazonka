{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.DescribeCodeCoverages
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves one or more code coverage reports.
--
-- This operation returns paginated results.
module Network.AWS.CodeBuild.DescribeCodeCoverages
  ( -- * Creating a request
    DescribeCodeCoverages (..),
    mkDescribeCodeCoverages,

    -- ** Request lenses
    dccMinLineCoveragePercentage,
    dccSortOrder,
    dccMaxLineCoveragePercentage,
    dccNextToken,
    dccMaxResults,
    dccSortBy,
    dccReportARN,

    -- * Destructuring the response
    DescribeCodeCoveragesResponse (..),
    mkDescribeCodeCoveragesResponse,

    -- ** Response lenses
    dccrsCodeCoverages,
    dccrsNextToken,
    dccrsResponseStatus,
  )
where

import Network.AWS.CodeBuild.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeCodeCoverages' smart constructor.
data DescribeCodeCoverages = DescribeCodeCoverages'
  { minLineCoveragePercentage ::
      Lude.Maybe Lude.Double,
    sortOrder :: Lude.Maybe SortOrderType,
    maxLineCoveragePercentage ::
      Lude.Maybe Lude.Double,
    nextToken :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    sortBy ::
      Lude.Maybe ReportCodeCoverageSortByType,
    reportARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeCodeCoverages' with the minimum fields required to make a request.
--
-- * 'maxLineCoveragePercentage' - The maximum line coverage percentage to report.
-- * 'maxResults' - The maximum number of results to return.
-- * 'minLineCoveragePercentage' - The minimum line coverage percentage to report.
-- * 'nextToken' - The @nextToken@ value returned from a previous call to @DescribeCodeCoverages@ . This specifies the next item to return. To return the beginning of the list, exclude this parameter.
-- * 'reportARN' - The ARN of the report for which test cases are returned.
-- * 'sortBy' - Specifies how the results are sorted. Possible values are:
--
--
--     * FILE_PATH
--
--     * The results are sorted by file path.
--
--
--     * LINE_COVERAGE_PERCENTAGE
--
--     * The results are sorted by the percentage of lines that are covered.
--
--
-- * 'sortOrder' - Specifies if the results are sorted in ascending or descending order.
mkDescribeCodeCoverages ::
  -- | 'reportARN'
  Lude.Text ->
  DescribeCodeCoverages
mkDescribeCodeCoverages pReportARN_ =
  DescribeCodeCoverages'
    { minLineCoveragePercentage = Lude.Nothing,
      sortOrder = Lude.Nothing,
      maxLineCoveragePercentage = Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing,
      sortBy = Lude.Nothing,
      reportARN = pReportARN_
    }

-- | The minimum line coverage percentage to report.
--
-- /Note:/ Consider using 'minLineCoveragePercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccMinLineCoveragePercentage :: Lens.Lens' DescribeCodeCoverages (Lude.Maybe Lude.Double)
dccMinLineCoveragePercentage = Lens.lens (minLineCoveragePercentage :: DescribeCodeCoverages -> Lude.Maybe Lude.Double) (\s a -> s {minLineCoveragePercentage = a} :: DescribeCodeCoverages)
{-# DEPRECATED dccMinLineCoveragePercentage "Use generic-lens or generic-optics with 'minLineCoveragePercentage' instead." #-}

-- | Specifies if the results are sorted in ascending or descending order.
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccSortOrder :: Lens.Lens' DescribeCodeCoverages (Lude.Maybe SortOrderType)
dccSortOrder = Lens.lens (sortOrder :: DescribeCodeCoverages -> Lude.Maybe SortOrderType) (\s a -> s {sortOrder = a} :: DescribeCodeCoverages)
{-# DEPRECATED dccSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

-- | The maximum line coverage percentage to report.
--
-- /Note:/ Consider using 'maxLineCoveragePercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccMaxLineCoveragePercentage :: Lens.Lens' DescribeCodeCoverages (Lude.Maybe Lude.Double)
dccMaxLineCoveragePercentage = Lens.lens (maxLineCoveragePercentage :: DescribeCodeCoverages -> Lude.Maybe Lude.Double) (\s a -> s {maxLineCoveragePercentage = a} :: DescribeCodeCoverages)
{-# DEPRECATED dccMaxLineCoveragePercentage "Use generic-lens or generic-optics with 'maxLineCoveragePercentage' instead." #-}

-- | The @nextToken@ value returned from a previous call to @DescribeCodeCoverages@ . This specifies the next item to return. To return the beginning of the list, exclude this parameter.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccNextToken :: Lens.Lens' DescribeCodeCoverages (Lude.Maybe Lude.Text)
dccNextToken = Lens.lens (nextToken :: DescribeCodeCoverages -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeCodeCoverages)
{-# DEPRECATED dccNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of results to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccMaxResults :: Lens.Lens' DescribeCodeCoverages (Lude.Maybe Lude.Natural)
dccMaxResults = Lens.lens (maxResults :: DescribeCodeCoverages -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: DescribeCodeCoverages)
{-# DEPRECATED dccMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Specifies how the results are sorted. Possible values are:
--
--
--     * FILE_PATH
--
--     * The results are sorted by file path.
--
--
--     * LINE_COVERAGE_PERCENTAGE
--
--     * The results are sorted by the percentage of lines that are covered.
--
--
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccSortBy :: Lens.Lens' DescribeCodeCoverages (Lude.Maybe ReportCodeCoverageSortByType)
dccSortBy = Lens.lens (sortBy :: DescribeCodeCoverages -> Lude.Maybe ReportCodeCoverageSortByType) (\s a -> s {sortBy = a} :: DescribeCodeCoverages)
{-# DEPRECATED dccSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

-- | The ARN of the report for which test cases are returned.
--
-- /Note:/ Consider using 'reportARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccReportARN :: Lens.Lens' DescribeCodeCoverages Lude.Text
dccReportARN = Lens.lens (reportARN :: DescribeCodeCoverages -> Lude.Text) (\s a -> s {reportARN = a} :: DescribeCodeCoverages)
{-# DEPRECATED dccReportARN "Use generic-lens or generic-optics with 'reportARN' instead." #-}

instance Page.AWSPager DescribeCodeCoverages where
  page rq rs
    | Page.stop (rs Lens.^. dccrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. dccrsCodeCoverages) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dccNextToken Lens..~ rs Lens.^. dccrsNextToken

instance Lude.AWSRequest DescribeCodeCoverages where
  type Rs DescribeCodeCoverages = DescribeCodeCoveragesResponse
  request = Req.postJSON codeBuildService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeCodeCoveragesResponse'
            Lude.<$> (x Lude..?> "codeCoverages" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeCodeCoverages where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("CodeBuild_20161006.DescribeCodeCoverages" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeCodeCoverages where
  toJSON DescribeCodeCoverages' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("minLineCoveragePercentage" Lude..=)
              Lude.<$> minLineCoveragePercentage,
            ("sortOrder" Lude..=) Lude.<$> sortOrder,
            ("maxLineCoveragePercentage" Lude..=)
              Lude.<$> maxLineCoveragePercentage,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("maxResults" Lude..=) Lude.<$> maxResults,
            ("sortBy" Lude..=) Lude.<$> sortBy,
            Lude.Just ("reportArn" Lude..= reportARN)
          ]
      )

instance Lude.ToPath DescribeCodeCoverages where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeCodeCoverages where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeCodeCoveragesResponse' smart constructor.
data DescribeCodeCoveragesResponse = DescribeCodeCoveragesResponse'
  { codeCoverages ::
      Lude.Maybe [CodeCoverage],
    nextToken ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribeCodeCoveragesResponse' with the minimum fields required to make a request.
--
-- * 'codeCoverages' - An array of @CodeCoverage@ objects that contain the results.
-- * 'nextToken' - If there are more items to return, this contains a token that is passed to a subsequent call to @DescribeCodeCoverages@ to retrieve the next set of items.
-- * 'responseStatus' - The response status code.
mkDescribeCodeCoveragesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeCodeCoveragesResponse
mkDescribeCodeCoveragesResponse pResponseStatus_ =
  DescribeCodeCoveragesResponse'
    { codeCoverages = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of @CodeCoverage@ objects that contain the results.
--
-- /Note:/ Consider using 'codeCoverages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccrsCodeCoverages :: Lens.Lens' DescribeCodeCoveragesResponse (Lude.Maybe [CodeCoverage])
dccrsCodeCoverages = Lens.lens (codeCoverages :: DescribeCodeCoveragesResponse -> Lude.Maybe [CodeCoverage]) (\s a -> s {codeCoverages = a} :: DescribeCodeCoveragesResponse)
{-# DEPRECATED dccrsCodeCoverages "Use generic-lens or generic-optics with 'codeCoverages' instead." #-}

-- | If there are more items to return, this contains a token that is passed to a subsequent call to @DescribeCodeCoverages@ to retrieve the next set of items.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccrsNextToken :: Lens.Lens' DescribeCodeCoveragesResponse (Lude.Maybe Lude.Text)
dccrsNextToken = Lens.lens (nextToken :: DescribeCodeCoveragesResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeCodeCoveragesResponse)
{-# DEPRECATED dccrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccrsResponseStatus :: Lens.Lens' DescribeCodeCoveragesResponse Lude.Int
dccrsResponseStatus = Lens.lens (responseStatus :: DescribeCodeCoveragesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeCodeCoveragesResponse)
{-# DEPRECATED dccrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
