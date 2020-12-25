{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.GetAggregateDiscoveredResourceCounts
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the resource counts across accounts and regions that are present in your AWS Config aggregator. You can request the resource counts by providing filters and GroupByKey.
--
-- For example, if the input contains accountID 12345678910 and region us-east-1 in filters, the API returns the count of resources in account ID 12345678910 and region us-east-1. If the input contains ACCOUNT_ID as a GroupByKey, the API returns resource counts for all source accounts that are present in your aggregator.
module Network.AWS.Config.GetAggregateDiscoveredResourceCounts
  ( -- * Creating a request
    GetAggregateDiscoveredResourceCounts (..),
    mkGetAggregateDiscoveredResourceCounts,

    -- ** Request lenses
    gadrcConfigurationAggregatorName,
    gadrcFilters,
    gadrcGroupByKey,
    gadrcLimit,
    gadrcNextToken,

    -- * Destructuring the response
    GetAggregateDiscoveredResourceCountsResponse (..),
    mkGetAggregateDiscoveredResourceCountsResponse,

    -- ** Response lenses
    gadrcrrsTotalDiscoveredResources,
    gadrcrrsGroupByKey,
    gadrcrrsGroupedResourceCounts,
    gadrcrrsNextToken,
    gadrcrrsResponseStatus,
  )
where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetAggregateDiscoveredResourceCounts' smart constructor.
data GetAggregateDiscoveredResourceCounts = GetAggregateDiscoveredResourceCounts'
  { -- | The name of the configuration aggregator.
    configurationAggregatorName :: Types.ConfigurationAggregatorName,
    -- | Filters the results based on the @ResourceCountFilters@ object.
    filters :: Core.Maybe Types.ResourceCountFilters,
    -- | The key to group the resource counts.
    groupByKey :: Core.Maybe Types.ResourceCountGroupKey,
    -- | The maximum number of 'GroupedResourceCount' objects returned on each page. The default is 1000. You cannot specify a number greater than 1000. If you specify 0, AWS Config uses the default.
    limit :: Core.Maybe Core.Natural,
    -- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAggregateDiscoveredResourceCounts' value with any optional fields omitted.
mkGetAggregateDiscoveredResourceCounts ::
  -- | 'configurationAggregatorName'
  Types.ConfigurationAggregatorName ->
  GetAggregateDiscoveredResourceCounts
mkGetAggregateDiscoveredResourceCounts configurationAggregatorName =
  GetAggregateDiscoveredResourceCounts'
    { configurationAggregatorName,
      filters = Core.Nothing,
      groupByKey = Core.Nothing,
      limit = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The name of the configuration aggregator.
--
-- /Note:/ Consider using 'configurationAggregatorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrcConfigurationAggregatorName :: Lens.Lens' GetAggregateDiscoveredResourceCounts Types.ConfigurationAggregatorName
gadrcConfigurationAggregatorName = Lens.field @"configurationAggregatorName"
{-# DEPRECATED gadrcConfigurationAggregatorName "Use generic-lens or generic-optics with 'configurationAggregatorName' instead." #-}

-- | Filters the results based on the @ResourceCountFilters@ object.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrcFilters :: Lens.Lens' GetAggregateDiscoveredResourceCounts (Core.Maybe Types.ResourceCountFilters)
gadrcFilters = Lens.field @"filters"
{-# DEPRECATED gadrcFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The key to group the resource counts.
--
-- /Note:/ Consider using 'groupByKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrcGroupByKey :: Lens.Lens' GetAggregateDiscoveredResourceCounts (Core.Maybe Types.ResourceCountGroupKey)
gadrcGroupByKey = Lens.field @"groupByKey"
{-# DEPRECATED gadrcGroupByKey "Use generic-lens or generic-optics with 'groupByKey' instead." #-}

-- | The maximum number of 'GroupedResourceCount' objects returned on each page. The default is 1000. You cannot specify a number greater than 1000. If you specify 0, AWS Config uses the default.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrcLimit :: Lens.Lens' GetAggregateDiscoveredResourceCounts (Core.Maybe Core.Natural)
gadrcLimit = Lens.field @"limit"
{-# DEPRECATED gadrcLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrcNextToken :: Lens.Lens' GetAggregateDiscoveredResourceCounts (Core.Maybe Types.NextToken)
gadrcNextToken = Lens.field @"nextToken"
{-# DEPRECATED gadrcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON GetAggregateDiscoveredResourceCounts where
  toJSON GetAggregateDiscoveredResourceCounts {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "ConfigurationAggregatorName"
                  Core..= configurationAggregatorName
              ),
            ("Filters" Core..=) Core.<$> filters,
            ("GroupByKey" Core..=) Core.<$> groupByKey,
            ("Limit" Core..=) Core.<$> limit,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest GetAggregateDiscoveredResourceCounts where
  type
    Rs GetAggregateDiscoveredResourceCounts =
      GetAggregateDiscoveredResourceCountsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "StarlingDoveService.GetAggregateDiscoveredResourceCounts"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAggregateDiscoveredResourceCountsResponse'
            Core.<$> (x Core..: "TotalDiscoveredResources")
            Core.<*> (x Core..:? "GroupByKey")
            Core.<*> (x Core..:? "GroupedResourceCounts")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetAggregateDiscoveredResourceCountsResponse' smart constructor.
data GetAggregateDiscoveredResourceCountsResponse = GetAggregateDiscoveredResourceCountsResponse'
  { -- | The total number of resources that are present in an aggregator with the filters that you provide.
    totalDiscoveredResources :: Core.Integer,
    -- | The key passed into the request object. If @GroupByKey@ is not provided, the result will be empty.
    groupByKey :: Core.Maybe Types.StringWithCharLimit256,
    -- | Returns a list of GroupedResourceCount objects.
    groupedResourceCounts :: Core.Maybe [Types.GroupedResourceCount],
    -- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAggregateDiscoveredResourceCountsResponse' value with any optional fields omitted.
mkGetAggregateDiscoveredResourceCountsResponse ::
  -- | 'totalDiscoveredResources'
  Core.Integer ->
  -- | 'responseStatus'
  Core.Int ->
  GetAggregateDiscoveredResourceCountsResponse
mkGetAggregateDiscoveredResourceCountsResponse
  totalDiscoveredResources
  responseStatus =
    GetAggregateDiscoveredResourceCountsResponse'
      { totalDiscoveredResources,
        groupByKey = Core.Nothing,
        groupedResourceCounts = Core.Nothing,
        nextToken = Core.Nothing,
        responseStatus
      }

-- | The total number of resources that are present in an aggregator with the filters that you provide.
--
-- /Note:/ Consider using 'totalDiscoveredResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrcrrsTotalDiscoveredResources :: Lens.Lens' GetAggregateDiscoveredResourceCountsResponse Core.Integer
gadrcrrsTotalDiscoveredResources = Lens.field @"totalDiscoveredResources"
{-# DEPRECATED gadrcrrsTotalDiscoveredResources "Use generic-lens or generic-optics with 'totalDiscoveredResources' instead." #-}

-- | The key passed into the request object. If @GroupByKey@ is not provided, the result will be empty.
--
-- /Note:/ Consider using 'groupByKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrcrrsGroupByKey :: Lens.Lens' GetAggregateDiscoveredResourceCountsResponse (Core.Maybe Types.StringWithCharLimit256)
gadrcrrsGroupByKey = Lens.field @"groupByKey"
{-# DEPRECATED gadrcrrsGroupByKey "Use generic-lens or generic-optics with 'groupByKey' instead." #-}

-- | Returns a list of GroupedResourceCount objects.
--
-- /Note:/ Consider using 'groupedResourceCounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrcrrsGroupedResourceCounts :: Lens.Lens' GetAggregateDiscoveredResourceCountsResponse (Core.Maybe [Types.GroupedResourceCount])
gadrcrrsGroupedResourceCounts = Lens.field @"groupedResourceCounts"
{-# DEPRECATED gadrcrrsGroupedResourceCounts "Use generic-lens or generic-optics with 'groupedResourceCounts' instead." #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrcrrsNextToken :: Lens.Lens' GetAggregateDiscoveredResourceCountsResponse (Core.Maybe Types.NextToken)
gadrcrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gadrcrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrcrrsResponseStatus :: Lens.Lens' GetAggregateDiscoveredResourceCountsResponse Core.Int
gadrcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gadrcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
