{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DescribeMetricCollectionTypes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the available CloudWatch metrics for Amazon EC2 Auto Scaling.
--
-- The @GroupStandbyInstances@ metric is not returned by default. You must explicitly request this metric when calling the 'EnableMetricsCollection' API.
module Network.AWS.AutoScaling.DescribeMetricCollectionTypes
    (
    -- * Creating a request
      DescribeMetricCollectionTypes (..)
    , mkDescribeMetricCollectionTypes

    -- * Destructuring the response
    , DescribeMetricCollectionTypesResponse (..)
    , mkDescribeMetricCollectionTypesResponse
    -- ** Response lenses
    , dmctrrsGranularities
    , dmctrrsMetrics
    , dmctrrsResponseStatus
    ) where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeMetricCollectionTypes' smart constructor.
data DescribeMetricCollectionTypes = DescribeMetricCollectionTypes'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeMetricCollectionTypes' value with any optional fields omitted.
mkDescribeMetricCollectionTypes
    :: DescribeMetricCollectionTypes
mkDescribeMetricCollectionTypes = DescribeMetricCollectionTypes'

instance Core.ToQuery DescribeMetricCollectionTypes where
        toQuery DescribeMetricCollectionTypes{..}
          = Core.toQueryPair "Action"
              ("DescribeMetricCollectionTypes" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2011-01-01" :: Core.Text)

instance Core.ToHeaders DescribeMetricCollectionTypes where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DescribeMetricCollectionTypes where
        type Rs DescribeMetricCollectionTypes =
             DescribeMetricCollectionTypesResponse
        toRequest x@_
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.mempty,
                         Core._rqHeaders =
                           Core.pure
                             ("Content-Type",
                              "application/x-www-form-urlencoded; charset=utf-8")
                             Core.<> Core.toHeaders x,
                         Core._rqBody = Core.toFormBody (Core.toQuery x)}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveXMLWrapper "DescribeMetricCollectionTypesResult"
              (\ s h x ->
                 DescribeMetricCollectionTypesResponse' Core.<$>
                   (x Core..@? "Granularities" Core..<@> Core.parseXMLList "member")
                     Core.<*> x Core..@? "Metrics" Core..<@> Core.parseXMLList "member"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeMetricCollectionTypesResponse' smart constructor.
data DescribeMetricCollectionTypesResponse = DescribeMetricCollectionTypesResponse'
  { granularities :: Core.Maybe [Types.MetricGranularityType]
    -- ^ The granularities for the metrics.
  , metrics :: Core.Maybe [Types.MetricCollectionType]
    -- ^ One or more metrics.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeMetricCollectionTypesResponse' value with any optional fields omitted.
mkDescribeMetricCollectionTypesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeMetricCollectionTypesResponse
mkDescribeMetricCollectionTypesResponse responseStatus
  = DescribeMetricCollectionTypesResponse'{granularities =
                                             Core.Nothing,
                                           metrics = Core.Nothing, responseStatus}

-- | The granularities for the metrics.
--
-- /Note:/ Consider using 'granularities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmctrrsGranularities :: Lens.Lens' DescribeMetricCollectionTypesResponse (Core.Maybe [Types.MetricGranularityType])
dmctrrsGranularities = Lens.field @"granularities"
{-# INLINEABLE dmctrrsGranularities #-}
{-# DEPRECATED granularities "Use generic-lens or generic-optics with 'granularities' instead"  #-}

-- | One or more metrics.
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmctrrsMetrics :: Lens.Lens' DescribeMetricCollectionTypesResponse (Core.Maybe [Types.MetricCollectionType])
dmctrrsMetrics = Lens.field @"metrics"
{-# INLINEABLE dmctrrsMetrics #-}
{-# DEPRECATED metrics "Use generic-lens or generic-optics with 'metrics' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmctrrsResponseStatus :: Lens.Lens' DescribeMetricCollectionTypesResponse Core.Int
dmctrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dmctrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
