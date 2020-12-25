{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ListContributorInsights (..),
    mkListContributorInsights,

    -- ** Request lenses
    lciMaxResults,
    lciNextToken,
    lciTableName,

    -- * Destructuring the response
    ListContributorInsightsResponse (..),
    mkListContributorInsightsResponse,

    -- ** Response lenses
    lcirrsContributorInsightsSummaries,
    lcirrsNextToken,
    lcirrsResponseStatus,
  )
where

import qualified Network.AWS.DynamoDB.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListContributorInsights' smart constructor.
data ListContributorInsights = ListContributorInsights'
  { -- | Maximum number of results to return per page.
    maxResults :: Core.Maybe Core.Int,
    -- | A token to for the desired page, if there is one.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The name of the table.
    tableName :: Core.Maybe Types.TableName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListContributorInsights' value with any optional fields omitted.
mkListContributorInsights ::
  ListContributorInsights
mkListContributorInsights =
  ListContributorInsights'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      tableName = Core.Nothing
    }

-- | Maximum number of results to return per page.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciMaxResults :: Lens.Lens' ListContributorInsights (Core.Maybe Core.Int)
lciMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lciMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A token to for the desired page, if there is one.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciNextToken :: Lens.Lens' ListContributorInsights (Core.Maybe Types.NextToken)
lciNextToken = Lens.field @"nextToken"
{-# DEPRECATED lciNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The name of the table.
--
-- /Note:/ Consider using 'tableName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lciTableName :: Lens.Lens' ListContributorInsights (Core.Maybe Types.TableName)
lciTableName = Lens.field @"tableName"
{-# DEPRECATED lciTableName "Use generic-lens or generic-optics with 'tableName' instead." #-}

instance Core.FromJSON ListContributorInsights where
  toJSON ListContributorInsights {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("TableName" Core..=) Core.<$> tableName
          ]
      )

instance Core.AWSRequest ListContributorInsights where
  type Rs ListContributorInsights = ListContributorInsightsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "DynamoDB_20120810.ListContributorInsights")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.0")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListContributorInsightsResponse'
            Core.<$> (x Core..:? "ContributorInsightsSummaries")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkListContributorInsightsResponse' smart constructor.
data ListContributorInsightsResponse = ListContributorInsightsResponse'
  { -- | A list of ContributorInsightsSummary.
    contributorInsightsSummaries :: Core.Maybe [Types.ContributorInsightsSummary],
    -- | A token to go to the next page if there is one.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListContributorInsightsResponse' value with any optional fields omitted.
mkListContributorInsightsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListContributorInsightsResponse
mkListContributorInsightsResponse responseStatus =
  ListContributorInsightsResponse'
    { contributorInsightsSummaries =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A list of ContributorInsightsSummary.
--
-- /Note:/ Consider using 'contributorInsightsSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcirrsContributorInsightsSummaries :: Lens.Lens' ListContributorInsightsResponse (Core.Maybe [Types.ContributorInsightsSummary])
lcirrsContributorInsightsSummaries = Lens.field @"contributorInsightsSummaries"
{-# DEPRECATED lcirrsContributorInsightsSummaries "Use generic-lens or generic-optics with 'contributorInsightsSummaries' instead." #-}

-- | A token to go to the next page if there is one.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcirrsNextToken :: Lens.Lens' ListContributorInsightsResponse (Core.Maybe Types.NextToken)
lcirrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lcirrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcirrsResponseStatus :: Lens.Lens' ListContributorInsightsResponse Core.Int
lcirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lcirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
