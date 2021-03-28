{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.GetSavingsPlansCoverage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the Savings Plans covered for your account. This enables you to see how much of your cost is covered by a Savings Plan. An organization’s management account can see the coverage of the associated member accounts. This supports dimensions, Cost Categories, and nested expressions. For any time period, you can filter data for Savings Plans usage with the following dimensions:
--
--
--     * @LINKED_ACCOUNT@ 
--
--
--     * @REGION@ 
--
--
--     * @SERVICE@ 
--
--
--     * @INSTANCE_FAMILY@ 
--
--
-- To determine valid values for a dimension, use the @GetDimensionValues@ operation.
module Network.AWS.CostExplorer.GetSavingsPlansCoverage
    (
    -- * Creating a request
      GetSavingsPlansCoverage (..)
    , mkGetSavingsPlansCoverage
    -- ** Request lenses
    , gspcTimePeriod
    , gspcFilter
    , gspcGranularity
    , gspcGroupBy
    , gspcMaxResults
    , gspcMetrics
    , gspcNextToken

    -- * Destructuring the response
    , GetSavingsPlansCoverageResponse (..)
    , mkGetSavingsPlansCoverageResponse
    -- ** Response lenses
    , gspcrrsSavingsPlansCoverages
    , gspcrrsNextToken
    , gspcrrsResponseStatus
    ) where

import qualified Network.AWS.CostExplorer.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetSavingsPlansCoverage' smart constructor.
data GetSavingsPlansCoverage = GetSavingsPlansCoverage'
  { timePeriod :: Types.DateInterval
    -- ^ The time period that you want the usage and costs for. The @Start@ date must be within 13 months. The @End@ date must be after the @Start@ date, and before the current date. Future dates can't be used as an @End@ date.
  , filter :: Core.Maybe Types.Expression
    -- ^ Filters Savings Plans coverage data by dimensions. You can filter data for Savings Plans usage with the following dimensions:
--
--
--     * @LINKED_ACCOUNT@ 
--
--
--     * @REGION@ 
--
--
--     * @SERVICE@ 
--
--
--     * @INSTANCE_FAMILY@ 
--
--
-- @GetSavingsPlansCoverage@ uses the same <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> object as the other operations, but only @AND@ is supported among each dimension. If there are multiple values for a dimension, they are OR'd together.
-- Cost category is also supported.
  , granularity :: Core.Maybe Types.Granularity
    -- ^ The granularity of the Amazon Web Services cost data for your Savings Plans. @Granularity@ can't be set if @GroupBy@ is set.
--
-- The @GetSavingsPlansCoverage@ operation supports only @DAILY@ and @MONTHLY@ granularities.
  , groupBy :: Core.Maybe [Types.GroupDefinition]
    -- ^ You can group the data using the attributes @INSTANCE_FAMILY@ , @REGION@ , or @SERVICE@ .
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The number of items to be returned in a response. The default is @20@ , with a minimum value of @1@ .
  , metrics :: Core.Maybe [Types.MetricName]
    -- ^ The measurement that you want your Savings Plans coverage reported in. The only valid value is @SpendCoveredBySavingsPlans@ .
  , nextToken :: Core.Maybe Types.NextPageToken
    -- ^ The token to retrieve the next set of results. Amazon Web Services provides the token when the response from a previous call has more results than the maximum page size.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSavingsPlansCoverage' value with any optional fields omitted.
mkGetSavingsPlansCoverage
    :: Types.DateInterval -- ^ 'timePeriod'
    -> GetSavingsPlansCoverage
mkGetSavingsPlansCoverage timePeriod
  = GetSavingsPlansCoverage'{timePeriod, filter = Core.Nothing,
                             granularity = Core.Nothing, groupBy = Core.Nothing,
                             maxResults = Core.Nothing, metrics = Core.Nothing,
                             nextToken = Core.Nothing}

-- | The time period that you want the usage and costs for. The @Start@ date must be within 13 months. The @End@ date must be after the @Start@ date, and before the current date. Future dates can't be used as an @End@ date.
--
-- /Note:/ Consider using 'timePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspcTimePeriod :: Lens.Lens' GetSavingsPlansCoverage Types.DateInterval
gspcTimePeriod = Lens.field @"timePeriod"
{-# INLINEABLE gspcTimePeriod #-}
{-# DEPRECATED timePeriod "Use generic-lens or generic-optics with 'timePeriod' instead"  #-}

-- | Filters Savings Plans coverage data by dimensions. You can filter data for Savings Plans usage with the following dimensions:
--
--
--     * @LINKED_ACCOUNT@ 
--
--
--     * @REGION@ 
--
--
--     * @SERVICE@ 
--
--
--     * @INSTANCE_FAMILY@ 
--
--
-- @GetSavingsPlansCoverage@ uses the same <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> object as the other operations, but only @AND@ is supported among each dimension. If there are multiple values for a dimension, they are OR'd together.
-- Cost category is also supported.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspcFilter :: Lens.Lens' GetSavingsPlansCoverage (Core.Maybe Types.Expression)
gspcFilter = Lens.field @"filter"
{-# INLINEABLE gspcFilter #-}
{-# DEPRECATED filter "Use generic-lens or generic-optics with 'filter' instead"  #-}

-- | The granularity of the Amazon Web Services cost data for your Savings Plans. @Granularity@ can't be set if @GroupBy@ is set.
--
-- The @GetSavingsPlansCoverage@ operation supports only @DAILY@ and @MONTHLY@ granularities.
--
-- /Note:/ Consider using 'granularity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspcGranularity :: Lens.Lens' GetSavingsPlansCoverage (Core.Maybe Types.Granularity)
gspcGranularity = Lens.field @"granularity"
{-# INLINEABLE gspcGranularity #-}
{-# DEPRECATED granularity "Use generic-lens or generic-optics with 'granularity' instead"  #-}

-- | You can group the data using the attributes @INSTANCE_FAMILY@ , @REGION@ , or @SERVICE@ .
--
-- /Note:/ Consider using 'groupBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspcGroupBy :: Lens.Lens' GetSavingsPlansCoverage (Core.Maybe [Types.GroupDefinition])
gspcGroupBy = Lens.field @"groupBy"
{-# INLINEABLE gspcGroupBy #-}
{-# DEPRECATED groupBy "Use generic-lens or generic-optics with 'groupBy' instead"  #-}

-- | The number of items to be returned in a response. The default is @20@ , with a minimum value of @1@ .
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspcMaxResults :: Lens.Lens' GetSavingsPlansCoverage (Core.Maybe Core.Natural)
gspcMaxResults = Lens.field @"maxResults"
{-# INLINEABLE gspcMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The measurement that you want your Savings Plans coverage reported in. The only valid value is @SpendCoveredBySavingsPlans@ .
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspcMetrics :: Lens.Lens' GetSavingsPlansCoverage (Core.Maybe [Types.MetricName])
gspcMetrics = Lens.field @"metrics"
{-# INLINEABLE gspcMetrics #-}
{-# DEPRECATED metrics "Use generic-lens or generic-optics with 'metrics' instead"  #-}

-- | The token to retrieve the next set of results. Amazon Web Services provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspcNextToken :: Lens.Lens' GetSavingsPlansCoverage (Core.Maybe Types.NextPageToken)
gspcNextToken = Lens.field @"nextToken"
{-# INLINEABLE gspcNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery GetSavingsPlansCoverage where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetSavingsPlansCoverage where
        toHeaders GetSavingsPlansCoverage{..}
          = Core.pure
              ("X-Amz-Target", "AWSInsightsIndexService.GetSavingsPlansCoverage")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetSavingsPlansCoverage where
        toJSON GetSavingsPlansCoverage{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("TimePeriod" Core..= timePeriod),
                  ("Filter" Core..=) Core.<$> filter,
                  ("Granularity" Core..=) Core.<$> granularity,
                  ("GroupBy" Core..=) Core.<$> groupBy,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("Metrics" Core..=) Core.<$> metrics,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest GetSavingsPlansCoverage where
        type Rs GetSavingsPlansCoverage = GetSavingsPlansCoverageResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetSavingsPlansCoverageResponse' Core.<$>
                   (x Core..:? "SavingsPlansCoverages" Core..!= Core.mempty) Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetSavingsPlansCoverageResponse' smart constructor.
data GetSavingsPlansCoverageResponse = GetSavingsPlansCoverageResponse'
  { savingsPlansCoverages :: [Types.SavingsPlansCoverage]
    -- ^ The amount of spend that your Savings Plans covered.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to retrieve the next set of results. Amazon Web Services provides the token when the response from a previous call has more results than the maximum page size.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSavingsPlansCoverageResponse' value with any optional fields omitted.
mkGetSavingsPlansCoverageResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetSavingsPlansCoverageResponse
mkGetSavingsPlansCoverageResponse responseStatus
  = GetSavingsPlansCoverageResponse'{savingsPlansCoverages =
                                       Core.mempty,
                                     nextToken = Core.Nothing, responseStatus}

-- | The amount of spend that your Savings Plans covered.
--
-- /Note:/ Consider using 'savingsPlansCoverages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspcrrsSavingsPlansCoverages :: Lens.Lens' GetSavingsPlansCoverageResponse [Types.SavingsPlansCoverage]
gspcrrsSavingsPlansCoverages = Lens.field @"savingsPlansCoverages"
{-# INLINEABLE gspcrrsSavingsPlansCoverages #-}
{-# DEPRECATED savingsPlansCoverages "Use generic-lens or generic-optics with 'savingsPlansCoverages' instead"  #-}

-- | The token to retrieve the next set of results. Amazon Web Services provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspcrrsNextToken :: Lens.Lens' GetSavingsPlansCoverageResponse (Core.Maybe Types.NextToken)
gspcrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gspcrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspcrrsResponseStatus :: Lens.Lens' GetSavingsPlansCoverageResponse Core.Int
gspcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gspcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
