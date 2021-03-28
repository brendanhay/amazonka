{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeCodeCoverages (..)
    , mkDescribeCodeCoverages
    -- ** Request lenses
    , dccReportArn
    , dccMaxLineCoveragePercentage
    , dccMaxResults
    , dccMinLineCoveragePercentage
    , dccNextToken
    , dccSortBy
    , dccSortOrder

    -- * Destructuring the response
    , DescribeCodeCoveragesResponse (..)
    , mkDescribeCodeCoveragesResponse
    -- ** Response lenses
    , dccrrsCodeCoverages
    , dccrrsNextToken
    , dccrrsResponseStatus
    ) where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeCodeCoverages' smart constructor.
data DescribeCodeCoverages = DescribeCodeCoverages'
  { reportArn :: Types.ReportArn
    -- ^ The ARN of the report for which test cases are returned. 
  , maxLineCoveragePercentage :: Core.Maybe Core.Double
    -- ^ The maximum line coverage percentage to report.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return.
  , minLineCoveragePercentage :: Core.Maybe Core.Double
    -- ^ The minimum line coverage percentage to report.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The @nextToken@ value returned from a previous call to @DescribeCodeCoverages@ . This specifies the next item to return. To return the beginning of the list, exclude this parameter.
  , sortBy :: Core.Maybe Types.ReportCodeCoverageSortByType
    -- ^ Specifies how the results are sorted. Possible values are:
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
  , sortOrder :: Core.Maybe Types.SortOrderType
    -- ^ Specifies if the results are sorted in ascending or descending order.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeCodeCoverages' value with any optional fields omitted.
mkDescribeCodeCoverages
    :: Types.ReportArn -- ^ 'reportArn'
    -> DescribeCodeCoverages
mkDescribeCodeCoverages reportArn
  = DescribeCodeCoverages'{reportArn,
                           maxLineCoveragePercentage = Core.Nothing,
                           maxResults = Core.Nothing,
                           minLineCoveragePercentage = Core.Nothing, nextToken = Core.Nothing,
                           sortBy = Core.Nothing, sortOrder = Core.Nothing}

-- | The ARN of the report for which test cases are returned. 
--
-- /Note:/ Consider using 'reportArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccReportArn :: Lens.Lens' DescribeCodeCoverages Types.ReportArn
dccReportArn = Lens.field @"reportArn"
{-# INLINEABLE dccReportArn #-}
{-# DEPRECATED reportArn "Use generic-lens or generic-optics with 'reportArn' instead"  #-}

-- | The maximum line coverage percentage to report.
--
-- /Note:/ Consider using 'maxLineCoveragePercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccMaxLineCoveragePercentage :: Lens.Lens' DescribeCodeCoverages (Core.Maybe Core.Double)
dccMaxLineCoveragePercentage = Lens.field @"maxLineCoveragePercentage"
{-# INLINEABLE dccMaxLineCoveragePercentage #-}
{-# DEPRECATED maxLineCoveragePercentage "Use generic-lens or generic-optics with 'maxLineCoveragePercentage' instead"  #-}

-- | The maximum number of results to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccMaxResults :: Lens.Lens' DescribeCodeCoverages (Core.Maybe Core.Natural)
dccMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dccMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The minimum line coverage percentage to report.
--
-- /Note:/ Consider using 'minLineCoveragePercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccMinLineCoveragePercentage :: Lens.Lens' DescribeCodeCoverages (Core.Maybe Core.Double)
dccMinLineCoveragePercentage = Lens.field @"minLineCoveragePercentage"
{-# INLINEABLE dccMinLineCoveragePercentage #-}
{-# DEPRECATED minLineCoveragePercentage "Use generic-lens or generic-optics with 'minLineCoveragePercentage' instead"  #-}

-- | The @nextToken@ value returned from a previous call to @DescribeCodeCoverages@ . This specifies the next item to return. To return the beginning of the list, exclude this parameter.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccNextToken :: Lens.Lens' DescribeCodeCoverages (Core.Maybe Core.Text)
dccNextToken = Lens.field @"nextToken"
{-# INLINEABLE dccNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

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
{-# INLINEABLE dccSortBy #-}
{-# DEPRECATED sortBy "Use generic-lens or generic-optics with 'sortBy' instead"  #-}

-- | Specifies if the results are sorted in ascending or descending order.
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccSortOrder :: Lens.Lens' DescribeCodeCoverages (Core.Maybe Types.SortOrderType)
dccSortOrder = Lens.field @"sortOrder"
{-# INLINEABLE dccSortOrder #-}
{-# DEPRECATED sortOrder "Use generic-lens or generic-optics with 'sortOrder' instead"  #-}

instance Core.ToQuery DescribeCodeCoverages where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeCodeCoverages where
        toHeaders DescribeCodeCoverages{..}
          = Core.pure
              ("X-Amz-Target", "CodeBuild_20161006.DescribeCodeCoverages")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeCodeCoverages where
        toJSON DescribeCodeCoverages{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("reportArn" Core..= reportArn),
                  ("maxLineCoveragePercentage" Core..=) Core.<$>
                    maxLineCoveragePercentage,
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("minLineCoveragePercentage" Core..=) Core.<$>
                    minLineCoveragePercentage,
                  ("nextToken" Core..=) Core.<$> nextToken,
                  ("sortBy" Core..=) Core.<$> sortBy,
                  ("sortOrder" Core..=) Core.<$> sortOrder])

instance Core.AWSRequest DescribeCodeCoverages where
        type Rs DescribeCodeCoverages = DescribeCodeCoveragesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeCodeCoveragesResponse' Core.<$>
                   (x Core..:? "codeCoverages") Core.<*> x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeCodeCoverages where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"codeCoverages" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeCodeCoveragesResponse' smart constructor.
data DescribeCodeCoveragesResponse = DescribeCodeCoveragesResponse'
  { codeCoverages :: Core.Maybe [Types.CodeCoverage]
    -- ^ An array of @CodeCoverage@ objects that contain the results.
  , nextToken :: Core.Maybe Core.Text
    -- ^ If there are more items to return, this contains a token that is passed to a subsequent call to @DescribeCodeCoverages@ to retrieve the next set of items.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeCodeCoveragesResponse' value with any optional fields omitted.
mkDescribeCodeCoveragesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeCodeCoveragesResponse
mkDescribeCodeCoveragesResponse responseStatus
  = DescribeCodeCoveragesResponse'{codeCoverages = Core.Nothing,
                                   nextToken = Core.Nothing, responseStatus}

-- | An array of @CodeCoverage@ objects that contain the results.
--
-- /Note:/ Consider using 'codeCoverages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccrrsCodeCoverages :: Lens.Lens' DescribeCodeCoveragesResponse (Core.Maybe [Types.CodeCoverage])
dccrrsCodeCoverages = Lens.field @"codeCoverages"
{-# INLINEABLE dccrrsCodeCoverages #-}
{-# DEPRECATED codeCoverages "Use generic-lens or generic-optics with 'codeCoverages' instead"  #-}

-- | If there are more items to return, this contains a token that is passed to a subsequent call to @DescribeCodeCoverages@ to retrieve the next set of items.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccrrsNextToken :: Lens.Lens' DescribeCodeCoveragesResponse (Core.Maybe Core.Text)
dccrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dccrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dccrrsResponseStatus :: Lens.Lens' DescribeCodeCoveragesResponse Core.Int
dccrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dccrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
