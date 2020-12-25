{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.GetOpsSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- View a summary of OpsItems based on specified filters and aggregators.
--
-- This operation returns paginated results.
module Network.AWS.SSM.GetOpsSummary
  ( -- * Creating a request
    GetOpsSummary (..),
    mkGetOpsSummary,

    -- ** Request lenses
    gosAggregators,
    gosFilters,
    gosMaxResults,
    gosNextToken,
    gosResultAttributes,
    gosSyncName,

    -- * Destructuring the response
    GetOpsSummaryResponse (..),
    mkGetOpsSummaryResponse,

    -- ** Response lenses
    gosrrsEntities,
    gosrrsNextToken,
    gosrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkGetOpsSummary' smart constructor.
data GetOpsSummary = GetOpsSummary'
  { -- | Optional aggregators that return counts of OpsItems based on one or more expressions.
    aggregators :: Core.Maybe (Core.NonEmpty Types.OpsAggregator),
    -- | Optional filters used to scope down the returned OpsItems.
    filters :: Core.Maybe (Core.NonEmpty Types.OpsFilter),
    -- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | A token to start the list. Use this token to get the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The OpsItem data type to return.
    resultAttributes :: Core.Maybe (Core.NonEmpty Types.OpsResultAttribute),
    -- | Specify the name of a resource data sync to get.
    syncName :: Core.Maybe Types.SyncName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetOpsSummary' value with any optional fields omitted.
mkGetOpsSummary ::
  GetOpsSummary
mkGetOpsSummary =
  GetOpsSummary'
    { aggregators = Core.Nothing,
      filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      resultAttributes = Core.Nothing,
      syncName = Core.Nothing
    }

-- | Optional aggregators that return counts of OpsItems based on one or more expressions.
--
-- /Note:/ Consider using 'aggregators' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gosAggregators :: Lens.Lens' GetOpsSummary (Core.Maybe (Core.NonEmpty Types.OpsAggregator))
gosAggregators = Lens.field @"aggregators"
{-# DEPRECATED gosAggregators "Use generic-lens or generic-optics with 'aggregators' instead." #-}

-- | Optional filters used to scope down the returned OpsItems.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gosFilters :: Lens.Lens' GetOpsSummary (Core.Maybe (Core.NonEmpty Types.OpsFilter))
gosFilters = Lens.field @"filters"
{-# DEPRECATED gosFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gosMaxResults :: Lens.Lens' GetOpsSummary (Core.Maybe Core.Natural)
gosMaxResults = Lens.field @"maxResults"
{-# DEPRECATED gosMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | A token to start the list. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gosNextToken :: Lens.Lens' GetOpsSummary (Core.Maybe Types.NextToken)
gosNextToken = Lens.field @"nextToken"
{-# DEPRECATED gosNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The OpsItem data type to return.
--
-- /Note:/ Consider using 'resultAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gosResultAttributes :: Lens.Lens' GetOpsSummary (Core.Maybe (Core.NonEmpty Types.OpsResultAttribute))
gosResultAttributes = Lens.field @"resultAttributes"
{-# DEPRECATED gosResultAttributes "Use generic-lens or generic-optics with 'resultAttributes' instead." #-}

-- | Specify the name of a resource data sync to get.
--
-- /Note:/ Consider using 'syncName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gosSyncName :: Lens.Lens' GetOpsSummary (Core.Maybe Types.SyncName)
gosSyncName = Lens.field @"syncName"
{-# DEPRECATED gosSyncName "Use generic-lens or generic-optics with 'syncName' instead." #-}

instance Core.FromJSON GetOpsSummary where
  toJSON GetOpsSummary {..} =
    Core.object
      ( Core.catMaybes
          [ ("Aggregators" Core..=) Core.<$> aggregators,
            ("Filters" Core..=) Core.<$> filters,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("ResultAttributes" Core..=) Core.<$> resultAttributes,
            ("SyncName" Core..=) Core.<$> syncName
          ]
      )

instance Core.AWSRequest GetOpsSummary where
  type Rs GetOpsSummary = GetOpsSummaryResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.GetOpsSummary")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetOpsSummaryResponse'
            Core.<$> (x Core..:? "Entities")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetOpsSummary where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"entities" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkGetOpsSummaryResponse' smart constructor.
data GetOpsSummaryResponse = GetOpsSummaryResponse'
  { -- | The list of aggregated and filtered OpsItems.
    entities :: Core.Maybe [Types.OpsEntity],
    -- | The token for the next set of items to return. Use this token to get the next set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetOpsSummaryResponse' value with any optional fields omitted.
mkGetOpsSummaryResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetOpsSummaryResponse
mkGetOpsSummaryResponse responseStatus =
  GetOpsSummaryResponse'
    { entities = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The list of aggregated and filtered OpsItems.
--
-- /Note:/ Consider using 'entities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gosrrsEntities :: Lens.Lens' GetOpsSummaryResponse (Core.Maybe [Types.OpsEntity])
gosrrsEntities = Lens.field @"entities"
{-# DEPRECATED gosrrsEntities "Use generic-lens or generic-optics with 'entities' instead." #-}

-- | The token for the next set of items to return. Use this token to get the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gosrrsNextToken :: Lens.Lens' GetOpsSummaryResponse (Core.Maybe Types.NextToken)
gosrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gosrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gosrrsResponseStatus :: Lens.Lens' GetOpsSummaryResponse Core.Int
gosrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gosrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
