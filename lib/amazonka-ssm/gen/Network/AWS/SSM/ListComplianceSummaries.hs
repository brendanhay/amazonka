{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.ListComplianceSummaries
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a summary count of compliant and non-compliant resources for a compliance type. For example, this call can return State Manager associations, patches, or custom compliance types according to the filter criteria that you specify.
--
-- This operation returns paginated results.
module Network.AWS.SSM.ListComplianceSummaries
  ( -- * Creating a request
    ListComplianceSummaries (..),
    mkListComplianceSummaries,

    -- ** Request lenses
    lcsFilters,
    lcsMaxResults,
    lcsNextToken,

    -- * Destructuring the response
    ListComplianceSummariesResponse (..),
    mkListComplianceSummariesResponse,

    -- ** Response lenses
    lcsrrsComplianceSummaryItems,
    lcsrrsNextToken,
    lcsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkListComplianceSummaries' smart constructor.
data ListComplianceSummaries = ListComplianceSummaries'
  { -- | One or more compliance or inventory filters. Use a filter to return a more specific list of results.
    filters :: Core.Maybe [Types.ComplianceStringFilter],
    -- | The maximum number of items to return for this call. Currently, you can specify null or 50. The call also returns a token that you can specify in a subsequent call to get the next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | A token to start the list. Use this token to get the next set of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListComplianceSummaries' value with any optional fields omitted.
mkListComplianceSummaries ::
  ListComplianceSummaries
mkListComplianceSummaries =
  ListComplianceSummaries'
    { filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | One or more compliance or inventory filters. Use a filter to return a more specific list of results.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsFilters :: Lens.Lens' ListComplianceSummaries (Core.Maybe [Types.ComplianceStringFilter])
lcsFilters = Lens.field @"filters"
{-# DEPRECATED lcsFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of items to return for this call. Currently, you can specify null or 50. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsMaxResults :: Lens.Lens' ListComplianceSummaries (Core.Maybe Core.Natural)
lcsMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lcsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A token to start the list. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsNextToken :: Lens.Lens' ListComplianceSummaries (Core.Maybe Types.NextToken)
lcsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lcsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListComplianceSummaries where
  toJSON ListComplianceSummaries {..} =
    Core.object
      ( Core.catMaybes
          [ ("Filters" Core..=) Core.<$> filters,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListComplianceSummaries where
  type Rs ListComplianceSummaries = ListComplianceSummariesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.ListComplianceSummaries")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListComplianceSummariesResponse'
            Core.<$> (x Core..:? "ComplianceSummaryItems")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListComplianceSummaries where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"complianceSummaryItems" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListComplianceSummariesResponse' smart constructor.
data ListComplianceSummariesResponse = ListComplianceSummariesResponse'
  { -- | A list of compliant and non-compliant summary counts based on compliance types. For example, this call returns State Manager associations, patches, or custom compliance types according to the filter criteria that you specified.
    complianceSummaryItems :: Core.Maybe [Types.ComplianceSummaryItem],
    -- | The token for the next set of items to return. Use this token to get the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListComplianceSummariesResponse' value with any optional fields omitted.
mkListComplianceSummariesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListComplianceSummariesResponse
mkListComplianceSummariesResponse responseStatus =
  ListComplianceSummariesResponse'
    { complianceSummaryItems =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of compliant and non-compliant summary counts based on compliance types. For example, this call returns State Manager associations, patches, or custom compliance types according to the filter criteria that you specified.
--
-- /Note:/ Consider using 'complianceSummaryItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsrrsComplianceSummaryItems :: Lens.Lens' ListComplianceSummariesResponse (Core.Maybe [Types.ComplianceSummaryItem])
lcsrrsComplianceSummaryItems = Lens.field @"complianceSummaryItems"
{-# DEPRECATED lcsrrsComplianceSummaryItems "Use generic-lens or generic-optics with 'complianceSummaryItems' instead." #-}

-- | The token for the next set of items to return. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsrrsNextToken :: Lens.Lens' ListComplianceSummariesResponse (Core.Maybe Types.NextToken)
lcsrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lcsrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsrrsResponseStatus :: Lens.Lens' ListComplianceSummariesResponse Core.Int
lcsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lcsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
