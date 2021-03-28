{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.GetAggregateConfigRuleComplianceSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the number of compliant and noncompliant rules for one or more accounts and regions in an aggregator.
module Network.AWS.Config.GetAggregateConfigRuleComplianceSummary
    (
    -- * Creating a request
      GetAggregateConfigRuleComplianceSummary (..)
    , mkGetAggregateConfigRuleComplianceSummary
    -- ** Request lenses
    , gacrcsConfigurationAggregatorName
    , gacrcsFilters
    , gacrcsGroupByKey
    , gacrcsLimit
    , gacrcsNextToken

    -- * Destructuring the response
    , GetAggregateConfigRuleComplianceSummaryResponse (..)
    , mkGetAggregateConfigRuleComplianceSummaryResponse
    -- ** Response lenses
    , gacrcsrrsAggregateComplianceCounts
    , gacrcsrrsGroupByKey
    , gacrcsrrsNextToken
    , gacrcsrrsResponseStatus
    ) where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetAggregateConfigRuleComplianceSummary' smart constructor.
data GetAggregateConfigRuleComplianceSummary = GetAggregateConfigRuleComplianceSummary'
  { configurationAggregatorName :: Types.ConfigurationAggregatorName
    -- ^ The name of the configuration aggregator.
  , filters :: Core.Maybe Types.ConfigRuleComplianceSummaryFilters
    -- ^ Filters the results based on the ConfigRuleComplianceSummaryFilters object.
  , groupByKey :: Core.Maybe Types.ConfigRuleComplianceSummaryGroupKey
    -- ^ Groups the result based on ACCOUNT_ID or AWS_REGION.
  , limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of evaluation results returned on each page. The default is 1000. You cannot specify a number greater than 1000. If you specify 0, AWS Config uses the default.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetAggregateConfigRuleComplianceSummary' value with any optional fields omitted.
mkGetAggregateConfigRuleComplianceSummary
    :: Types.ConfigurationAggregatorName -- ^ 'configurationAggregatorName'
    -> GetAggregateConfigRuleComplianceSummary
mkGetAggregateConfigRuleComplianceSummary
  configurationAggregatorName
  = GetAggregateConfigRuleComplianceSummary'{configurationAggregatorName,
                                             filters = Core.Nothing, groupByKey = Core.Nothing,
                                             limit = Core.Nothing, nextToken = Core.Nothing}

-- | The name of the configuration aggregator.
--
-- /Note:/ Consider using 'configurationAggregatorName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacrcsConfigurationAggregatorName :: Lens.Lens' GetAggregateConfigRuleComplianceSummary Types.ConfigurationAggregatorName
gacrcsConfigurationAggregatorName = Lens.field @"configurationAggregatorName"
{-# INLINEABLE gacrcsConfigurationAggregatorName #-}
{-# DEPRECATED configurationAggregatorName "Use generic-lens or generic-optics with 'configurationAggregatorName' instead"  #-}

-- | Filters the results based on the ConfigRuleComplianceSummaryFilters object.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacrcsFilters :: Lens.Lens' GetAggregateConfigRuleComplianceSummary (Core.Maybe Types.ConfigRuleComplianceSummaryFilters)
gacrcsFilters = Lens.field @"filters"
{-# INLINEABLE gacrcsFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | Groups the result based on ACCOUNT_ID or AWS_REGION.
--
-- /Note:/ Consider using 'groupByKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacrcsGroupByKey :: Lens.Lens' GetAggregateConfigRuleComplianceSummary (Core.Maybe Types.ConfigRuleComplianceSummaryGroupKey)
gacrcsGroupByKey = Lens.field @"groupByKey"
{-# INLINEABLE gacrcsGroupByKey #-}
{-# DEPRECATED groupByKey "Use generic-lens or generic-optics with 'groupByKey' instead"  #-}

-- | The maximum number of evaluation results returned on each page. The default is 1000. You cannot specify a number greater than 1000. If you specify 0, AWS Config uses the default.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacrcsLimit :: Lens.Lens' GetAggregateConfigRuleComplianceSummary (Core.Maybe Core.Natural)
gacrcsLimit = Lens.field @"limit"
{-# INLINEABLE gacrcsLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacrcsNextToken :: Lens.Lens' GetAggregateConfigRuleComplianceSummary (Core.Maybe Types.NextToken)
gacrcsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gacrcsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery GetAggregateConfigRuleComplianceSummary where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetAggregateConfigRuleComplianceSummary
         where
        toHeaders GetAggregateConfigRuleComplianceSummary{..}
          = Core.pure
              ("X-Amz-Target",
               "StarlingDoveService.GetAggregateConfigRuleComplianceSummary")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetAggregateConfigRuleComplianceSummary
         where
        toJSON GetAggregateConfigRuleComplianceSummary{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("ConfigurationAggregatorName" Core..=
                       configurationAggregatorName),
                  ("Filters" Core..=) Core.<$> filters,
                  ("GroupByKey" Core..=) Core.<$> groupByKey,
                  ("Limit" Core..=) Core.<$> limit,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest GetAggregateConfigRuleComplianceSummary
         where
        type Rs GetAggregateConfigRuleComplianceSummary =
             GetAggregateConfigRuleComplianceSummaryResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetAggregateConfigRuleComplianceSummaryResponse' Core.<$>
                   (x Core..:? "AggregateComplianceCounts") Core.<*>
                     x Core..:? "GroupByKey"
                     Core.<*> x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetAggregateConfigRuleComplianceSummaryResponse' smart constructor.
data GetAggregateConfigRuleComplianceSummaryResponse = GetAggregateConfigRuleComplianceSummaryResponse'
  { aggregateComplianceCounts :: Core.Maybe [Types.AggregateComplianceCount]
    -- ^ Returns a list of AggregateComplianceCounts object.
  , groupByKey :: Core.Maybe Types.StringWithCharLimit256
    -- ^ Groups the result based on ACCOUNT_ID or AWS_REGION.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetAggregateConfigRuleComplianceSummaryResponse' value with any optional fields omitted.
mkGetAggregateConfigRuleComplianceSummaryResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetAggregateConfigRuleComplianceSummaryResponse
mkGetAggregateConfigRuleComplianceSummaryResponse responseStatus
  = GetAggregateConfigRuleComplianceSummaryResponse'{aggregateComplianceCounts
                                                       = Core.Nothing,
                                                     groupByKey = Core.Nothing,
                                                     nextToken = Core.Nothing, responseStatus}

-- | Returns a list of AggregateComplianceCounts object.
--
-- /Note:/ Consider using 'aggregateComplianceCounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacrcsrrsAggregateComplianceCounts :: Lens.Lens' GetAggregateConfigRuleComplianceSummaryResponse (Core.Maybe [Types.AggregateComplianceCount])
gacrcsrrsAggregateComplianceCounts = Lens.field @"aggregateComplianceCounts"
{-# INLINEABLE gacrcsrrsAggregateComplianceCounts #-}
{-# DEPRECATED aggregateComplianceCounts "Use generic-lens or generic-optics with 'aggregateComplianceCounts' instead"  #-}

-- | Groups the result based on ACCOUNT_ID or AWS_REGION.
--
-- /Note:/ Consider using 'groupByKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacrcsrrsGroupByKey :: Lens.Lens' GetAggregateConfigRuleComplianceSummaryResponse (Core.Maybe Types.StringWithCharLimit256)
gacrcsrrsGroupByKey = Lens.field @"groupByKey"
{-# INLINEABLE gacrcsrrsGroupByKey #-}
{-# DEPRECATED groupByKey "Use generic-lens or generic-optics with 'groupByKey' instead"  #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacrcsrrsNextToken :: Lens.Lens' GetAggregateConfigRuleComplianceSummaryResponse (Core.Maybe Types.NextToken)
gacrcsrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gacrcsrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gacrcsrrsResponseStatus :: Lens.Lens' GetAggregateConfigRuleComplianceSummaryResponse Core.Int
gacrcsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gacrcsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
