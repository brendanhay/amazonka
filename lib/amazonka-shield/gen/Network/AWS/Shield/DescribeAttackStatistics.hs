{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.DescribeAttackStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about the number and type of attacks AWS Shield has detected in the last year for all resources that belong to your account, regardless of whether you've defined Shield protections for them. This operation is available to Shield customers as well as to Shield Advanced customers.
--
-- The operation returns data for the time range of midnight UTC, one year ago, to midnight UTC, today. For example, if the current time is @2020-10-26 15:39:32 PDT@ , equal to @2020-10-26 22:39:32 UTC@ , then the time range for the attack data returned is from @2019-10-26 00:00:00 UTC@ to @2020-10-26 00:00:00 UTC@ . 
-- The time range indicates the period covered by the attack statistics data items.
module Network.AWS.Shield.DescribeAttackStatistics
    (
    -- * Creating a request
      DescribeAttackStatistics (..)
    , mkDescribeAttackStatistics

    -- * Destructuring the response
    , DescribeAttackStatisticsResponse (..)
    , mkDescribeAttackStatisticsResponse
    -- ** Response lenses
    , dasrrsTimeRange
    , dasrrsDataItems
    , dasrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Shield.Types as Types

-- | /See:/ 'mkDescribeAttackStatistics' smart constructor.
data DescribeAttackStatistics = DescribeAttackStatistics'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeAttackStatistics' value with any optional fields omitted.
mkDescribeAttackStatistics
    :: DescribeAttackStatistics
mkDescribeAttackStatistics = DescribeAttackStatistics'

instance Core.ToQuery DescribeAttackStatistics where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeAttackStatistics where
        toHeaders DescribeAttackStatistics{..}
          = Core.pure
              ("X-Amz-Target", "AWSShield_20160616.DescribeAttackStatistics")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeAttackStatistics where
        toJSON _ = Core.Object Core.mempty

instance Core.AWSRequest DescribeAttackStatistics where
        type Rs DescribeAttackStatistics = DescribeAttackStatisticsResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeAttackStatisticsResponse' Core.<$>
                   (x Core..: "TimeRange") Core.<*>
                     x Core..:? "DataItems" Core..!= Core.mempty
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeAttackStatisticsResponse' smart constructor.
data DescribeAttackStatisticsResponse = DescribeAttackStatisticsResponse'
  { timeRange :: Types.TimeRange
  , dataItems :: [Types.AttackStatisticsDataItem]
    -- ^ The data that describes the attacks detected during the time period.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeAttackStatisticsResponse' value with any optional fields omitted.
mkDescribeAttackStatisticsResponse
    :: Types.TimeRange -- ^ 'timeRange'
    -> Core.Int -- ^ 'responseStatus'
    -> DescribeAttackStatisticsResponse
mkDescribeAttackStatisticsResponse timeRange responseStatus
  = DescribeAttackStatisticsResponse'{timeRange,
                                      dataItems = Core.mempty, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'timeRange' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrrsTimeRange :: Lens.Lens' DescribeAttackStatisticsResponse Types.TimeRange
dasrrsTimeRange = Lens.field @"timeRange"
{-# INLINEABLE dasrrsTimeRange #-}
{-# DEPRECATED timeRange "Use generic-lens or generic-optics with 'timeRange' instead"  #-}

-- | The data that describes the attacks detected during the time period.
--
-- /Note:/ Consider using 'dataItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrrsDataItems :: Lens.Lens' DescribeAttackStatisticsResponse [Types.AttackStatisticsDataItem]
dasrrsDataItems = Lens.field @"dataItems"
{-# INLINEABLE dasrrsDataItems #-}
{-# DEPRECATED dataItems "Use generic-lens or generic-optics with 'dataItems' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dasrrsResponseStatus :: Lens.Lens' DescribeAttackStatisticsResponse Core.Int
dasrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dasrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
