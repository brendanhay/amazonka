{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.GetInventory
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Query inventory information.
--
-- This operation returns paginated results.
module Network.AWS.SSM.GetInventory
  ( -- * Creating a request
    GetInventory (..),
    mkGetInventory,

    -- ** Request lenses
    giAggregators,
    giFilters,
    giMaxResults,
    giNextToken,
    giResultAttributes,

    -- * Destructuring the response
    GetInventoryResponse (..),
    mkGetInventoryResponse,

    -- ** Response lenses
    girrsEntities,
    girrsNextToken,
    girrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkGetInventory' smart constructor.
data GetInventory = GetInventory'
  { -- | Returns counts of inventory types based on one or more expressions. For example, if you aggregate by using an expression that uses the @AWS:InstanceInformation.PlatformType@ type, you can see a count of how many Windows and Linux instances exist in your inventoried fleet.
    aggregators :: Core.Maybe (Core.NonEmpty Types.InventoryAggregator),
    -- | One or more filters. Use a filter to return a more specific list of results.
    filters :: Core.Maybe (Core.NonEmpty Types.InventoryFilter),
    -- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token for the next set of items to return. (You received this token from a previous call.)
    nextToken :: Core.Maybe Types.NextToken,
    -- | The list of inventory item types to return.
    resultAttributes :: Core.Maybe (Core.NonEmpty Types.ResultAttribute)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetInventory' value with any optional fields omitted.
mkGetInventory ::
  GetInventory
mkGetInventory =
  GetInventory'
    { aggregators = Core.Nothing,
      filters = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      resultAttributes = Core.Nothing
    }

-- | Returns counts of inventory types based on one or more expressions. For example, if you aggregate by using an expression that uses the @AWS:InstanceInformation.PlatformType@ type, you can see a count of how many Windows and Linux instances exist in your inventoried fleet.
--
-- /Note:/ Consider using 'aggregators' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giAggregators :: Lens.Lens' GetInventory (Core.Maybe (Core.NonEmpty Types.InventoryAggregator))
giAggregators = Lens.field @"aggregators"
{-# DEPRECATED giAggregators "Use generic-lens or generic-optics with 'aggregators' instead." #-}

-- | One or more filters. Use a filter to return a more specific list of results.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giFilters :: Lens.Lens' GetInventory (Core.Maybe (Core.NonEmpty Types.InventoryFilter))
giFilters = Lens.field @"filters"
{-# DEPRECATED giFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giMaxResults :: Lens.Lens' GetInventory (Core.Maybe Core.Natural)
giMaxResults = Lens.field @"maxResults"
{-# DEPRECATED giMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giNextToken :: Lens.Lens' GetInventory (Core.Maybe Types.NextToken)
giNextToken = Lens.field @"nextToken"
{-# DEPRECATED giNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The list of inventory item types to return.
--
-- /Note:/ Consider using 'resultAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
giResultAttributes :: Lens.Lens' GetInventory (Core.Maybe (Core.NonEmpty Types.ResultAttribute))
giResultAttributes = Lens.field @"resultAttributes"
{-# DEPRECATED giResultAttributes "Use generic-lens or generic-optics with 'resultAttributes' instead." #-}

instance Core.FromJSON GetInventory where
  toJSON GetInventory {..} =
    Core.object
      ( Core.catMaybes
          [ ("Aggregators" Core..=) Core.<$> aggregators,
            ("Filters" Core..=) Core.<$> filters,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("ResultAttributes" Core..=) Core.<$> resultAttributes
          ]
      )

instance Core.AWSRequest GetInventory where
  type Rs GetInventory = GetInventoryResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonSSM.GetInventory")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInventoryResponse'
            Core.<$> (x Core..:? "Entities")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetInventory where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"entities" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkGetInventoryResponse' smart constructor.
data GetInventoryResponse = GetInventoryResponse'
  { -- | Collection of inventory entities such as a collection of instance inventory.
    entities :: Core.Maybe [Types.InventoryResultEntity],
    -- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetInventoryResponse' value with any optional fields omitted.
mkGetInventoryResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetInventoryResponse
mkGetInventoryResponse responseStatus =
  GetInventoryResponse'
    { entities = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Collection of inventory entities such as a collection of instance inventory.
--
-- /Note:/ Consider using 'entities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrsEntities :: Lens.Lens' GetInventoryResponse (Core.Maybe [Types.InventoryResultEntity])
girrsEntities = Lens.field @"entities"
{-# DEPRECATED girrsEntities "Use generic-lens or generic-optics with 'entities' instead." #-}

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrsNextToken :: Lens.Lens' GetInventoryResponse (Core.Maybe Types.NextToken)
girrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED girrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
girrsResponseStatus :: Lens.Lens' GetInventoryResponse Core.Int
girrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED girrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
