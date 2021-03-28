{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.GetUsageStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists Amazon GuardDuty usage statistics over the last 30 days for the specified detector ID. For newly enabled detectors or data sources the cost returned will include only the usage so far under 30 days, this may differ from the cost metrics in the console, which projects usage over 30 days to provide a monthly cost estimate. For more information see <https://docs.aws.amazon.com/guardduty/latest/ug/monitoring_costs.html#usage-calculations Understanding How Usage Costs are Calculated> .
module Network.AWS.GuardDuty.GetUsageStatistics
    (
    -- * Creating a request
      GetUsageStatistics (..)
    , mkGetUsageStatistics
    -- ** Request lenses
    , gusDetectorId
    , gusUsageStatisticType
    , gusUsageCriteria
    , gusMaxResults
    , gusNextToken
    , gusUnit

    -- * Destructuring the response
    , GetUsageStatisticsResponse (..)
    , mkGetUsageStatisticsResponse
    -- ** Response lenses
    , gusrrsNextToken
    , gusrrsUsageStatistics
    , gusrrsResponseStatus
    ) where

import qualified Network.AWS.GuardDuty.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetUsageStatistics' smart constructor.
data GetUsageStatistics = GetUsageStatistics'
  { detectorId :: Types.DetectorId
    -- ^ The ID of the detector that specifies the GuardDuty service whose usage statistics you want to retrieve.
  , usageStatisticType :: Types.UsageStatisticType
    -- ^ The type of usage statistics to retrieve.
  , usageCriteria :: Types.UsageCriteria
    -- ^ Represents the criteria used for querying usage.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return in the response.
  , nextToken :: Core.Maybe Core.Text
    -- ^ A token to use for paginating results that are returned in the response. Set the value of this parameter to null for the first request to a list action. For subsequent calls, use the NextToken value returned from the previous request to continue listing results after the first page.
  , unit :: Core.Maybe Core.Text
    -- ^ The currency unit you would like to view your usage statistics in. Current valid values are USD.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetUsageStatistics' value with any optional fields omitted.
mkGetUsageStatistics
    :: Types.DetectorId -- ^ 'detectorId'
    -> Types.UsageStatisticType -- ^ 'usageStatisticType'
    -> Types.UsageCriteria -- ^ 'usageCriteria'
    -> GetUsageStatistics
mkGetUsageStatistics detectorId usageStatisticType usageCriteria
  = GetUsageStatistics'{detectorId, usageStatisticType,
                        usageCriteria, maxResults = Core.Nothing, nextToken = Core.Nothing,
                        unit = Core.Nothing}

-- | The ID of the detector that specifies the GuardDuty service whose usage statistics you want to retrieve.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gusDetectorId :: Lens.Lens' GetUsageStatistics Types.DetectorId
gusDetectorId = Lens.field @"detectorId"
{-# INLINEABLE gusDetectorId #-}
{-# DEPRECATED detectorId "Use generic-lens or generic-optics with 'detectorId' instead"  #-}

-- | The type of usage statistics to retrieve.
--
-- /Note:/ Consider using 'usageStatisticType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gusUsageStatisticType :: Lens.Lens' GetUsageStatistics Types.UsageStatisticType
gusUsageStatisticType = Lens.field @"usageStatisticType"
{-# INLINEABLE gusUsageStatisticType #-}
{-# DEPRECATED usageStatisticType "Use generic-lens or generic-optics with 'usageStatisticType' instead"  #-}

-- | Represents the criteria used for querying usage.
--
-- /Note:/ Consider using 'usageCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gusUsageCriteria :: Lens.Lens' GetUsageStatistics Types.UsageCriteria
gusUsageCriteria = Lens.field @"usageCriteria"
{-# INLINEABLE gusUsageCriteria #-}
{-# DEPRECATED usageCriteria "Use generic-lens or generic-optics with 'usageCriteria' instead"  #-}

-- | The maximum number of results to return in the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gusMaxResults :: Lens.Lens' GetUsageStatistics (Core.Maybe Core.Natural)
gusMaxResults = Lens.field @"maxResults"
{-# INLINEABLE gusMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A token to use for paginating results that are returned in the response. Set the value of this parameter to null for the first request to a list action. For subsequent calls, use the NextToken value returned from the previous request to continue listing results after the first page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gusNextToken :: Lens.Lens' GetUsageStatistics (Core.Maybe Core.Text)
gusNextToken = Lens.field @"nextToken"
{-# INLINEABLE gusNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The currency unit you would like to view your usage statistics in. Current valid values are USD.
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gusUnit :: Lens.Lens' GetUsageStatistics (Core.Maybe Core.Text)
gusUnit = Lens.field @"unit"
{-# INLINEABLE gusUnit #-}
{-# DEPRECATED unit "Use generic-lens or generic-optics with 'unit' instead"  #-}

instance Core.ToQuery GetUsageStatistics where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetUsageStatistics where
        toHeaders GetUsageStatistics{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetUsageStatistics where
        toJSON GetUsageStatistics{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("usageStatisticsType" Core..= usageStatisticType),
                  Core.Just ("usageCriteria" Core..= usageCriteria),
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken,
                  ("unit" Core..=) Core.<$> unit])

instance Core.AWSRequest GetUsageStatistics where
        type Rs GetUsageStatistics = GetUsageStatisticsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST,
                         Core._rqPath =
                           "/detector/" Core.<> Core.toText detectorId Core.<>
                             "/usage/statistics",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetUsageStatisticsResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "usageStatistics"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetUsageStatisticsResponse' smart constructor.
data GetUsageStatisticsResponse = GetUsageStatisticsResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The pagination parameter to be used on the next list operation to retrieve more items.
  , usageStatistics :: Core.Maybe Types.UsageStatistics
    -- ^ The usage statistics object. If a UsageStatisticType was provided, the objects representing other types will be null.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetUsageStatisticsResponse' value with any optional fields omitted.
mkGetUsageStatisticsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetUsageStatisticsResponse
mkGetUsageStatisticsResponse responseStatus
  = GetUsageStatisticsResponse'{nextToken = Core.Nothing,
                                usageStatistics = Core.Nothing, responseStatus}

-- | The pagination parameter to be used on the next list operation to retrieve more items.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gusrrsNextToken :: Lens.Lens' GetUsageStatisticsResponse (Core.Maybe Core.Text)
gusrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gusrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The usage statistics object. If a UsageStatisticType was provided, the objects representing other types will be null.
--
-- /Note:/ Consider using 'usageStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gusrrsUsageStatistics :: Lens.Lens' GetUsageStatisticsResponse (Core.Maybe Types.UsageStatistics)
gusrrsUsageStatistics = Lens.field @"usageStatistics"
{-# INLINEABLE gusrrsUsageStatistics #-}
{-# DEPRECATED usageStatistics "Use generic-lens or generic-optics with 'usageStatistics' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gusrrsResponseStatus :: Lens.Lens' GetUsageStatisticsResponse Core.Int
gusrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gusrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
