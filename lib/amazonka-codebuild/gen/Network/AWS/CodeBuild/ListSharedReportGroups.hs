{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeBuild.ListSharedReportGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of report groups that are shared with other AWS accounts or users. 
--
-- This operation returns paginated results.
module Network.AWS.CodeBuild.ListSharedReportGroups
    (
    -- * Creating a request
      ListSharedReportGroups (..)
    , mkListSharedReportGroups
    -- ** Request lenses
    , lsrgMaxResults
    , lsrgNextToken
    , lsrgSortBy
    , lsrgSortOrder

    -- * Destructuring the response
    , ListSharedReportGroupsResponse (..)
    , mkListSharedReportGroupsResponse
    -- ** Response lenses
    , lsrgrrsNextToken
    , lsrgrrsReportGroups
    , lsrgrrsResponseStatus
    ) where

import qualified Network.AWS.CodeBuild.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListSharedReportGroups' smart constructor.
data ListSharedReportGroups = ListSharedReportGroups'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of paginated shared report groups per response. Use @nextToken@ to iterate pages in the list of returned @ReportGroup@ objects. The default value is 100. 
  , nextToken :: Core.Maybe Core.Text
    -- ^ During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned. 
  , sortBy :: Core.Maybe Types.SharedResourceSortByType
    -- ^ The criterion to be used to list report groups shared with the current AWS account or user. Valid values include: 
--
--
--     * @ARN@ : List based on the ARN. 
--
--
--     * @MODIFIED_TIME@ : List based on when information about the shared report group was last changed. 
--
--
  , sortOrder :: Core.Maybe Types.SortOrderType
    -- ^ The order in which to list shared report groups. Valid values include:
--
--
--     * @ASCENDING@ : List in ascending order.
--
--
--     * @DESCENDING@ : List in descending order.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSharedReportGroups' value with any optional fields omitted.
mkListSharedReportGroups
    :: ListSharedReportGroups
mkListSharedReportGroups
  = ListSharedReportGroups'{maxResults = Core.Nothing,
                            nextToken = Core.Nothing, sortBy = Core.Nothing,
                            sortOrder = Core.Nothing}

-- | The maximum number of paginated shared report groups per response. Use @nextToken@ to iterate pages in the list of returned @ReportGroup@ objects. The default value is 100. 
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrgMaxResults :: Lens.Lens' ListSharedReportGroups (Core.Maybe Core.Natural)
lsrgMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lsrgMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrgNextToken :: Lens.Lens' ListSharedReportGroups (Core.Maybe Core.Text)
lsrgNextToken = Lens.field @"nextToken"
{-# INLINEABLE lsrgNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The criterion to be used to list report groups shared with the current AWS account or user. Valid values include: 
--
--
--     * @ARN@ : List based on the ARN. 
--
--
--     * @MODIFIED_TIME@ : List based on when information about the shared report group was last changed. 
--
--
--
-- /Note:/ Consider using 'sortBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrgSortBy :: Lens.Lens' ListSharedReportGroups (Core.Maybe Types.SharedResourceSortByType)
lsrgSortBy = Lens.field @"sortBy"
{-# INLINEABLE lsrgSortBy #-}
{-# DEPRECATED sortBy "Use generic-lens or generic-optics with 'sortBy' instead"  #-}

-- | The order in which to list shared report groups. Valid values include:
--
--
--     * @ASCENDING@ : List in ascending order.
--
--
--     * @DESCENDING@ : List in descending order.
--
--
--
-- /Note:/ Consider using 'sortOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrgSortOrder :: Lens.Lens' ListSharedReportGroups (Core.Maybe Types.SortOrderType)
lsrgSortOrder = Lens.field @"sortOrder"
{-# INLINEABLE lsrgSortOrder #-}
{-# DEPRECATED sortOrder "Use generic-lens or generic-optics with 'sortOrder' instead"  #-}

instance Core.ToQuery ListSharedReportGroups where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListSharedReportGroups where
        toHeaders ListSharedReportGroups{..}
          = Core.pure
              ("X-Amz-Target", "CodeBuild_20161006.ListSharedReportGroups")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListSharedReportGroups where
        toJSON ListSharedReportGroups{..}
          = Core.object
              (Core.catMaybes
                 [("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken,
                  ("sortBy" Core..=) Core.<$> sortBy,
                  ("sortOrder" Core..=) Core.<$> sortOrder])

instance Core.AWSRequest ListSharedReportGroups where
        type Rs ListSharedReportGroups = ListSharedReportGroupsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListSharedReportGroupsResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "reportGroups"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListSharedReportGroups where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"reportGroups" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListSharedReportGroupsResponse' smart constructor.
data ListSharedReportGroupsResponse = ListSharedReportGroupsResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned. 
  , reportGroups :: Core.Maybe (Core.NonEmpty Types.NonEmptyString)
    -- ^ The list of ARNs for the report groups shared with the current AWS account or user. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListSharedReportGroupsResponse' value with any optional fields omitted.
mkListSharedReportGroupsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListSharedReportGroupsResponse
mkListSharedReportGroupsResponse responseStatus
  = ListSharedReportGroupsResponse'{nextToken = Core.Nothing,
                                    reportGroups = Core.Nothing, responseStatus}

-- | During a previous call, the maximum number of items that can be returned is the value specified in @maxResults@ . If there more items in the list, then a unique string called a /nextToken/ is returned. To get the next batch of items in the list, call this operation again, adding the next token to the call. To get all of the items in the list, keep calling this operation with each subsequent next token that is returned, until no more next tokens are returned. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrgrrsNextToken :: Lens.Lens' ListSharedReportGroupsResponse (Core.Maybe Core.Text)
lsrgrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lsrgrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The list of ARNs for the report groups shared with the current AWS account or user. 
--
-- /Note:/ Consider using 'reportGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrgrrsReportGroups :: Lens.Lens' ListSharedReportGroupsResponse (Core.Maybe (Core.NonEmpty Types.NonEmptyString))
lsrgrrsReportGroups = Lens.field @"reportGroups"
{-# INLINEABLE lsrgrrsReportGroups #-}
{-# DEPRECATED reportGroups "Use generic-lens or generic-optics with 'reportGroups' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lsrgrrsResponseStatus :: Lens.Lens' ListSharedReportGroupsResponse Core.Int
lsrgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lsrgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
