{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    dccReportArn,
    dccMaxLineCoveragePercentage,
    dccMaxResults,
    dccMinLineCoveragePercentage,
    dccNextToken,
    dccSortBy,
    dccSortOrder,

    -- * Destructuring the response
    DescribeCodeCoveragesResponse (..),
    mkDescribeCodeCoveragesResponse,

    -- ** Response lenses
    dccrrsCodeCoverages,
    dccrrsNextToken,
    dccrrsResponseStatus,
  )
where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeCodeCoverages' smart constructor.
data DescribeCodeCoverages = DescribeCodeCoverages'
  { -- | The ARN of the report for which test cases are returned.
    reportArn :: Types.ReportArn,
    -- | The maximum line coverage percentage to report.
    maxLineCoveragePercentage :: Core.Maybe Core.Double,
    -- | The maximum number of results to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | The minimum line coverage percentage to report.
    minLineCoveragePercentage :: Core.Maybe Core.Double,
    -- | The @nextToken@ value returned from a previous call to @DescribeCodeCoverages@ . This specifies the next item to return. To return the beginning of the list, exclude this parameter.
    nextToken :: Core.Maybe Types.String,
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
    sortBy :: Core.Maybe Types.ReportCodeCoverageSortByType,
    -- | Specifies if the results are sorted in ascending or descending order.
    sortOrder :: Core.Maybe Types.SortOrderType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCodeCoverages' value with any optional fields omitted.
mkDescribeCodeCoverages ::
  -- | 'reportArn'
  Types.ReportArn ->
  DescribeCodeCoverages
mkDescribeCodeCoverages reportArn =
  DescribeCodeCoverages'
    { reportArn,
      maxLineCoveragePercentage = Core.Nothing,
      maxResults = Core.Nothing,
      minLineCoveragePercentage = Core.Nothing,
      nextToken = Core.Nothing,
      sortBy = Core.Nothing,
      sortOrder = Core.Nothing
    }

-- | The ARN of the report for which test cases are returned.
--
-- /Note:/ Consider using 'reportArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccReportArn :: Lens.Lens' DescribeCodeCoverages Types.ReportArn
dccReportArn = Lens.field @"reportArn"
{-# DEPRECATED dccReportArn "Use generic-lens or generic-optics with 'reportArn' instead." #-}

-- | The maximum line coverage percentage to report.
--
-- /Note:/ Consider using 'maxLineCoveragePercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccMaxLineCoveragePercentage :: Lens.Lens' DescribeCodeCoverages (Core.Maybe Core.Double)
dccMaxLineCoveragePercentage = Lens.field @"maxLineCoveragePercentage"
{-# DEPRECATED dccMaxLineCoveragePercentage "Use generic-lens or generic-optics with 'maxLineCoveragePercentage' instead." #-}

-- | The maximum number of results to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccMaxResults :: Lens.Lens' DescribeCodeCoverages (Core.Maybe Core.Natural)
dccMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dccMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The minimum line coverage percentage to report.
--
-- /Note:/ Consider using 'minLineCoveragePercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccMinLineCoveragePercentage :: Lens.Lens' DescribeCodeCoverages (Core.Maybe Core.Double)
dccMinLineCoveragePercentage = Lens.field @"minLineCoveragePercentage"
{-# DEPRECATED dccMinLineCoveragePercentage "Use generic-lens or generic-optics with 'minLineCoveragePercentage' instead." #-}

-- | The @nextToken@ value returned from a previous call to @DescribeCodeCoverages@ . This specifies the next item to return. To return the beginning of the list, exclude this parameter.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccNextToken :: Lens.Lens' DescribeCodeCoverages (Core.Maybe Types.String)
dccNextToken = Lens.field @"nextToken"
{-# DEPRECATED dccNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

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
dccSortBy :: Lens.Lens' DescribeCodeCoverages (Core.Maybe Types.ReportCodeCoverageSortByType)
dccSortBy = Lens.field @"sortBy"
{-# DEPRECATED dccSortBy "Use generic-lens or generic-optics with 'sortBy' instead." #-}

-- | Specifies if the results are sorted in ascending or descending order.
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccSortOrder :: Lens.Lens' DescribeCodeCoverages (Core.Maybe Types.SortOrderType)
dccSortOrder = Lens.field @"sortOrder"
{-# DEPRECATED dccSortOrder "Use generic-lens or generic-optics with 'sortOrder' instead." #-}

instance Core.FromJSON DescribeCodeCoverages where
  toJSON DescribeCodeCoverages {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("reportArn" Core..= reportArn),
            ("maxLineCoveragePercentage" Core..=)
              Core.<$> maxLineCoveragePercentage,
            ("maxResults" Core..=) Core.<$> maxResults,
            ("minLineCoveragePercentage" Core..=)
              Core.<$> minLineCoveragePercentage,
            ("nextToken" Core..=) Core.<$> nextToken,
            ("sortBy" Core..=) Core.<$> sortBy,
            ("sortOrder" Core..=) Core.<$> sortOrder
          ]
      )

instance Core.AWSRequest DescribeCodeCoverages where
  type Rs DescribeCodeCoverages = DescribeCodeCoveragesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "CodeBuild_20161006.DescribeCodeCoverages")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCodeCoveragesResponse'
            Core.<$> (x Core..:? "codeCoverages")
            Core.<*> (x Core..:? "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeCodeCoverages where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"codeCoverages" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkDescribeCodeCoveragesResponse' smart constructor.
data DescribeCodeCoveragesResponse = DescribeCodeCoveragesResponse'
  { -- | An array of @CodeCoverage@ objects that contain the results.
    codeCoverages :: Core.Maybe [Types.CodeCoverage],
    -- | If there are more items to return, this contains a token that is passed to a subsequent call to @DescribeCodeCoverages@ to retrieve the next set of items.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeCodeCoveragesResponse' value with any optional fields omitted.
mkDescribeCodeCoveragesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeCodeCoveragesResponse
mkDescribeCodeCoveragesResponse responseStatus =
  DescribeCodeCoveragesResponse'
    { codeCoverages = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | An array of @CodeCoverage@ objects that contain the results.
--
-- /Note:/ Consider using 'codeCoverages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccrrsCodeCoverages :: Lens.Lens' DescribeCodeCoveragesResponse (Core.Maybe [Types.CodeCoverage])
dccrrsCodeCoverages = Lens.field @"codeCoverages"
{-# DEPRECATED dccrrsCodeCoverages "Use generic-lens or generic-optics with 'codeCoverages' instead." #-}

-- | If there are more items to return, this contains a token that is passed to a subsequent call to @DescribeCodeCoverages@ to retrieve the next set of items.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccrrsNextToken :: Lens.Lens' DescribeCodeCoveragesResponse (Core.Maybe Types.String)
dccrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dccrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccrrsResponseStatus :: Lens.Lens' DescribeCodeCoveragesResponse Core.Int
dccrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dccrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
