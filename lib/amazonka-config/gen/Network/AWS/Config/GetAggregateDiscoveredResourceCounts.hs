{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetAggregateDiscoveredResourceCounts (..)
    , mkGetAggregateDiscoveredResourceCounts
    -- ** Request lenses
    , gadrcConfigurationAggregatorName
    , gadrcFilters
    , gadrcGroupByKey
    , gadrcLimit
    , gadrcNextToken

    -- * Destructuring the response
    , GetAggregateDiscoveredResourceCountsResponse (..)
    , mkGetAggregateDiscoveredResourceCountsResponse
    -- ** Response lenses
    , gadrcrrsTotalDiscoveredResources
    , gadrcrrsGroupByKey
    , gadrcrrsGroupedResourceCounts
    , gadrcrrsNextToken
    , gadrcrrsResponseStatus
    ) where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetAggregateDiscoveredResourceCounts' smart constructor.
data GetAggregateDiscoveredResourceCounts = GetAggregateDiscoveredResourceCounts'
  { configurationAggregatorName :: Types.ConfigurationAggregatorName
    -- ^ The name of the configuration aggregator.
  , filters :: Core.Maybe Types.ResourceCountFilters
    -- ^ Filters the results based on the @ResourceCountFilters@ object.
  , groupByKey :: Core.Maybe Types.ResourceCountGroupKey
    -- ^ The key to group the resource counts.
  , limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of 'GroupedResourceCount' objects returned on each page. The default is 1000. You cannot specify a number greater than 1000. If you specify 0, AWS Config uses the default.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAggregateDiscoveredResourceCounts' value with any optional fields omitted.
mkGetAggregateDiscoveredResourceCounts
    :: Types.ConfigurationAggregatorName -- ^ 'configurationAggregatorName'
    -> GetAggregateDiscoveredResourceCounts
mkGetAggregateDiscoveredResourceCounts configurationAggregatorName
  = GetAggregateDiscoveredResourceCounts'{configurationAggregatorName,
                                          filters = Core.Nothing, groupByKey = Core.Nothing,
                                          limit = Core.Nothing, nextToken = Core.Nothing}

-- | The name of the configuration aggregator.
--
-- /Note:/ Consider using 'configurationAggregatorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrcConfigurationAggregatorName :: Lens.Lens' GetAggregateDiscoveredResourceCounts Types.ConfigurationAggregatorName
gadrcConfigurationAggregatorName = Lens.field @"configurationAggregatorName"
{-# INLINEABLE gadrcConfigurationAggregatorName #-}
{-# DEPRECATED configurationAggregatorName "Use generic-lens or generic-optics with 'configurationAggregatorName' instead"  #-}

-- | Filters the results based on the @ResourceCountFilters@ object.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrcFilters :: Lens.Lens' GetAggregateDiscoveredResourceCounts (Core.Maybe Types.ResourceCountFilters)
gadrcFilters = Lens.field @"filters"
{-# INLINEABLE gadrcFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The key to group the resource counts.
--
-- /Note:/ Consider using 'groupByKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrcGroupByKey :: Lens.Lens' GetAggregateDiscoveredResourceCounts (Core.Maybe Types.ResourceCountGroupKey)
gadrcGroupByKey = Lens.field @"groupByKey"
{-# INLINEABLE gadrcGroupByKey #-}
{-# DEPRECATED groupByKey "Use generic-lens or generic-optics with 'groupByKey' instead"  #-}

-- | The maximum number of 'GroupedResourceCount' objects returned on each page. The default is 1000. You cannot specify a number greater than 1000. If you specify 0, AWS Config uses the default.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrcLimit :: Lens.Lens' GetAggregateDiscoveredResourceCounts (Core.Maybe Core.Natural)
gadrcLimit = Lens.field @"limit"
{-# INLINEABLE gadrcLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrcNextToken :: Lens.Lens' GetAggregateDiscoveredResourceCounts (Core.Maybe Types.NextToken)
gadrcNextToken = Lens.field @"nextToken"
{-# INLINEABLE gadrcNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery GetAggregateDiscoveredResourceCounts where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetAggregateDiscoveredResourceCounts where
        toHeaders GetAggregateDiscoveredResourceCounts{..}
          = Core.pure
              ("X-Amz-Target",
               "StarlingDoveService.GetAggregateDiscoveredResourceCounts")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetAggregateDiscoveredResourceCounts where
        toJSON GetAggregateDiscoveredResourceCounts{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("ConfigurationAggregatorName" Core..=
                       configurationAggregatorName),
                  ("Filters" Core..=) Core.<$> filters,
                  ("GroupByKey" Core..=) Core.<$> groupByKey,
                  ("Limit" Core..=) Core.<$> limit,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest GetAggregateDiscoveredResourceCounts where
        type Rs GetAggregateDiscoveredResourceCounts =
             GetAggregateDiscoveredResourceCountsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetAggregateDiscoveredResourceCountsResponse' Core.<$>
                   (x Core..: "TotalDiscoveredResources") Core.<*>
                     x Core..:? "GroupByKey"
                     Core.<*> x Core..:? "GroupedResourceCounts"
                     Core.<*> x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetAggregateDiscoveredResourceCountsResponse' smart constructor.
data GetAggregateDiscoveredResourceCountsResponse = GetAggregateDiscoveredResourceCountsResponse'
  { totalDiscoveredResources :: Core.Integer
    -- ^ The total number of resources that are present in an aggregator with the filters that you provide.
  , groupByKey :: Core.Maybe Types.StringWithCharLimit256
    -- ^ The key passed into the request object. If @GroupByKey@ is not provided, the result will be empty.
  , groupedResourceCounts :: Core.Maybe [Types.GroupedResourceCount]
    -- ^ Returns a list of GroupedResourceCount objects.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAggregateDiscoveredResourceCountsResponse' value with any optional fields omitted.
mkGetAggregateDiscoveredResourceCountsResponse
    :: Core.Integer -- ^ 'totalDiscoveredResources'
    -> Core.Int -- ^ 'responseStatus'
    -> GetAggregateDiscoveredResourceCountsResponse
mkGetAggregateDiscoveredResourceCountsResponse
  totalDiscoveredResources responseStatus
  = GetAggregateDiscoveredResourceCountsResponse'{totalDiscoveredResources,
                                                  groupByKey = Core.Nothing,
                                                  groupedResourceCounts = Core.Nothing,
                                                  nextToken = Core.Nothing, responseStatus}

-- | The total number of resources that are present in an aggregator with the filters that you provide.
--
-- /Note:/ Consider using 'totalDiscoveredResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrcrrsTotalDiscoveredResources :: Lens.Lens' GetAggregateDiscoveredResourceCountsResponse Core.Integer
gadrcrrsTotalDiscoveredResources = Lens.field @"totalDiscoveredResources"
{-# INLINEABLE gadrcrrsTotalDiscoveredResources #-}
{-# DEPRECATED totalDiscoveredResources "Use generic-lens or generic-optics with 'totalDiscoveredResources' instead"  #-}

-- | The key passed into the request object. If @GroupByKey@ is not provided, the result will be empty.
--
-- /Note:/ Consider using 'groupByKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrcrrsGroupByKey :: Lens.Lens' GetAggregateDiscoveredResourceCountsResponse (Core.Maybe Types.StringWithCharLimit256)
gadrcrrsGroupByKey = Lens.field @"groupByKey"
{-# INLINEABLE gadrcrrsGroupByKey #-}
{-# DEPRECATED groupByKey "Use generic-lens or generic-optics with 'groupByKey' instead"  #-}

-- | Returns a list of GroupedResourceCount objects.
--
-- /Note:/ Consider using 'groupedResourceCounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrcrrsGroupedResourceCounts :: Lens.Lens' GetAggregateDiscoveredResourceCountsResponse (Core.Maybe [Types.GroupedResourceCount])
gadrcrrsGroupedResourceCounts = Lens.field @"groupedResourceCounts"
{-# INLINEABLE gadrcrrsGroupedResourceCounts #-}
{-# DEPRECATED groupedResourceCounts "Use generic-lens or generic-optics with 'groupedResourceCounts' instead"  #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrcrrsNextToken :: Lens.Lens' GetAggregateDiscoveredResourceCountsResponse (Core.Maybe Types.NextToken)
gadrcrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gadrcrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gadrcrrsResponseStatus :: Lens.Lens' GetAggregateDiscoveredResourceCountsResponse Core.Int
gadrcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gadrcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
