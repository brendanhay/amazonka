{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.ListContributorInsights
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of ContributorInsightsSummary for a table and all its global secondary indexes.
module Network.AWS.DynamoDB.ListContributorInsights
    (
    -- * Creating a request
      ListContributorInsights (..)
    , mkListContributorInsights
    -- ** Request lenses
    , lciMaxResults
    , lciNextToken
    , lciTableName

    -- * Destructuring the response
    , ListContributorInsightsResponse (..)
    , mkListContributorInsightsResponse
    -- ** Response lenses
    , lcirrsContributorInsightsSummaries
    , lcirrsNextToken
    , lcirrsResponseStatus
    ) where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListContributorInsights' smart constructor.
data ListContributorInsights = ListContributorInsights'
  { maxResults :: Core.Maybe Core.Int
    -- ^ Maximum number of results to return per page.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A token to for the desired page, if there is one.
  , tableName :: Core.Maybe Types.TableName
    -- ^ The name of the table.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListContributorInsights' value with any optional fields omitted.
mkListContributorInsights
    :: ListContributorInsights
mkListContributorInsights
  = ListContributorInsights'{maxResults = Core.Nothing,
                             nextToken = Core.Nothing, tableName = Core.Nothing}

-- | Maximum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciMaxResults :: Lens.Lens' ListContributorInsights (Core.Maybe Core.Int)
lciMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lciMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A token to for the desired page, if there is one.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciNextToken :: Lens.Lens' ListContributorInsights (Core.Maybe Types.NextToken)
lciNextToken = Lens.field @"nextToken"
{-# INLINEABLE lciNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The name of the table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciTableName :: Lens.Lens' ListContributorInsights (Core.Maybe Types.TableName)
lciTableName = Lens.field @"tableName"
{-# INLINEABLE lciTableName #-}
{-# DEPRECATED tableName "Use generic-lens or generic-optics with 'tableName' instead"  #-}

instance Core.ToQuery ListContributorInsights where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListContributorInsights where
        toHeaders ListContributorInsights{..}
          = Core.pure
              ("X-Amz-Target", "DynamoDB_20120810.ListContributorInsights")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.0")

instance Core.FromJSON ListContributorInsights where
        toJSON ListContributorInsights{..}
          = Core.object
              (Core.catMaybes
                 [("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("TableName" Core..=) Core.<$> tableName])

instance Core.AWSRequest ListContributorInsights where
        type Rs ListContributorInsights = ListContributorInsightsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListContributorInsightsResponse' Core.<$>
                   (x Core..:? "ContributorInsightsSummaries") Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkListContributorInsightsResponse' smart constructor.
data ListContributorInsightsResponse = ListContributorInsightsResponse'
  { contributorInsightsSummaries :: Core.Maybe [Types.ContributorInsightsSummary]
    -- ^ A list of ContributorInsightsSummary.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ A token to go to the next page if there is one.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListContributorInsightsResponse' value with any optional fields omitted.
mkListContributorInsightsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListContributorInsightsResponse
mkListContributorInsightsResponse responseStatus
  = ListContributorInsightsResponse'{contributorInsightsSummaries =
                                       Core.Nothing,
                                     nextToken = Core.Nothing, responseStatus}

-- | A list of ContributorInsightsSummary.
--
-- /Note:/ Consider using 'contributorInsightsSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcirrsContributorInsightsSummaries :: Lens.Lens' ListContributorInsightsResponse (Core.Maybe [Types.ContributorInsightsSummary])
lcirrsContributorInsightsSummaries = Lens.field @"contributorInsightsSummaries"
{-# INLINEABLE lcirrsContributorInsightsSummaries #-}
{-# DEPRECATED contributorInsightsSummaries "Use generic-lens or generic-optics with 'contributorInsightsSummaries' instead"  #-}

-- | A token to go to the next page if there is one.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcirrsNextToken :: Lens.Lens' ListContributorInsightsResponse (Core.Maybe Types.NextToken)
lcirrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lcirrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcirrsResponseStatus :: Lens.Lens' ListContributorInsightsResponse Core.Int
lcirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lcirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
