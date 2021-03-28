{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ListReports (..)
    , mkListReports
    -- ** Request lenses
    , lrFilter
    , lrMaxResults
    , lrNextToken
    , lrSortOrder

    -- * Destructuring the response
    , ListReportsResponse (..)
    , mkListReportsResponse
    -- ** Response lenses
    , lrrrsNextToken
    , lrrrsReports
    , lrrrsResponseStatus
    ) where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListReports' smart constructor.
data ListReports = ListReports'
  { filter :: Core.Maybe Types.ReportFilter
    -- ^ A @ReportFilter@ object used to filter the returned reports. 
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of paginated reports returned per response. Use @nextToken@ to iterate pages in the list of returned @Report@ objects. The default value is 100. 
  , nextToken :: Core.Maybe Core.Text
    -- ^ During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned. 
  , sortOrder :: Core.Maybe Types.SortOrderType
    -- ^ Specifies the sort order for the list of returned reports. Valid values are: 
--
--
--     * @ASCENDING@ : return reports in chronological order based on their creation date. 
--
--
--     * @DESCENDING@ : return reports in the reverse chronological order based on their creation date. 
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListReports' value with any optional fields omitted.
mkListReports
    :: ListReports
mkListReports
  = ListReports'{filter = Core.Nothing, maxResults = Core.Nothing,
                 nextToken = Core.Nothing, sortOrder = Core.Nothing}

-- | A @ReportFilter@ object used to filter the returned reports. 
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrFilter :: Lens.Lens' ListReports (Core.Maybe Types.ReportFilter)
lrFilter = Lens.field @"filter"
{-# INLINEABLE lrFilter #-}
{-# DEPRECATED filter "Use generic-lens or generic-optics with 'filter' instead"  #-}

-- | The maximum number of paginated reports returned per response. Use @nextToken@ to iterate pages in the list of returned @Report@ objects. The default value is 100. 
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrMaxResults :: Lens.Lens' ListReports (Core.Maybe Core.Natural)
lrMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lrMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrNextToken :: Lens.Lens' ListReports (Core.Maybe Core.Text)
lrNextToken = Lens.field @"nextToken"
{-# INLINEABLE lrNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

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
lrSortOrder :: Lens.Lens' ListReports (Core.Maybe Types.SortOrderType)
lrSortOrder = Lens.field @"sortOrder"
{-# INLINEABLE lrSortOrder #-}
{-# DEPRECATED sortOrder "Use generic-lens or generic-optics with 'sortOrder' instead"  #-}

instance Core.ToQuery ListReports where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListReports where
        toHeaders ListReports{..}
          = Core.pure ("X-Amz-Target", "CodeBuild_20161006.ListReports")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListReports where
        toJSON ListReports{..}
          = Core.object
              (Core.catMaybes
                 [("filter" Core..=) Core.<$> filter,
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken,
                  ("sortOrder" Core..=) Core.<$> sortOrder])

instance Core.AWSRequest ListReports where
        type Rs ListReports = ListReportsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListReportsResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "reports" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListReports where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"reports" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListReportsResponse' smart constructor.
data ListReportsResponse = ListReportsResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned. 
  , reports :: Core.Maybe (Core.NonEmpty Types.NonEmptyString)
    -- ^ The list of returned ARNs for the reports in the current AWS account. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListReportsResponse' value with any optional fields omitted.
mkListReportsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListReportsResponse
mkListReportsResponse responseStatus
  = ListReportsResponse'{nextToken = Core.Nothing,
                         reports = Core.Nothing, responseStatus}

-- | During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrrsNextToken :: Lens.Lens' ListReportsResponse (Core.Maybe Core.Text)
lrrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lrrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The list of returned ARNs for the reports in the current AWS account. 
--
-- /Note:/ Consider using 'reports' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrrsReports :: Lens.Lens' ListReportsResponse (Core.Maybe (Core.NonEmpty Types.NonEmptyString))
lrrrsReports = Lens.field @"reports"
{-# INLINEABLE lrrrsReports #-}
{-# DEPRECATED reports "Use generic-lens or generic-optics with 'reports' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrrrsResponseStatus :: Lens.Lens' ListReportsResponse Core.Int
lrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
