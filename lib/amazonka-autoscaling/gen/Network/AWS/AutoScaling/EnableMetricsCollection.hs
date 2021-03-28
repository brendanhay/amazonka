{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.EnableMetricsCollection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables group metrics for the specified Auto Scaling group. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-monitoring.html Monitoring CloudWatch metrics for your Auto Scaling groups and instances> in the /Amazon EC2 Auto Scaling User Guide/ .
module Network.AWS.AutoScaling.EnableMetricsCollection
    (
    -- * Creating a request
      EnableMetricsCollection (..)
    , mkEnableMetricsCollection
    -- ** Request lenses
    , emcAutoScalingGroupName
    , emcGranularity
    , emcMetrics

    -- * Destructuring the response
    , EnableMetricsCollectionResponse (..)
    , mkEnableMetricsCollectionResponse
    ) where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkEnableMetricsCollection' smart constructor.
data EnableMetricsCollection = EnableMetricsCollection'
  { autoScalingGroupName :: Types.ResourceName
    -- ^ The name of the Auto Scaling group.
  , granularity :: Types.XmlStringMaxLen255
    -- ^ The granularity to associate with the metrics to collect. The only valid value is @1Minute@ .
  , metrics :: Core.Maybe [Types.XmlStringMaxLen255]
    -- ^ Specifies which group-level metrics to start collecting. You can specify one or more of the following metrics:
--
--
--     * @GroupMinSize@ 
--
--
--     * @GroupMaxSize@ 
--
--
--     * @GroupDesiredCapacity@ 
--
--
--     * @GroupInServiceInstances@ 
--
--
--     * @GroupPendingInstances@ 
--
--
--     * @GroupStandbyInstances@ 
--
--
--     * @GroupTerminatingInstances@ 
--
--
--     * @GroupTotalInstances@ 
--
--
-- The instance weighting feature supports the following additional metrics: 
--
--     * @GroupInServiceCapacity@ 
--
--
--     * @GroupPendingCapacity@ 
--
--
--     * @GroupStandbyCapacity@ 
--
--
--     * @GroupTerminatingCapacity@ 
--
--
--     * @GroupTotalCapacity@ 
--
--
-- If you omit this parameter, all metrics are enabled. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableMetricsCollection' value with any optional fields omitted.
mkEnableMetricsCollection
    :: Types.ResourceName -- ^ 'autoScalingGroupName'
    -> Types.XmlStringMaxLen255 -- ^ 'granularity'
    -> EnableMetricsCollection
mkEnableMetricsCollection autoScalingGroupName granularity
  = EnableMetricsCollection'{autoScalingGroupName, granularity,
                             metrics = Core.Nothing}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emcAutoScalingGroupName :: Lens.Lens' EnableMetricsCollection Types.ResourceName
emcAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# INLINEABLE emcAutoScalingGroupName #-}
{-# DEPRECATED autoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead"  #-}

-- | The granularity to associate with the metrics to collect. The only valid value is @1Minute@ .
--
-- /Note:/ Consider using 'granularity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emcGranularity :: Lens.Lens' EnableMetricsCollection Types.XmlStringMaxLen255
emcGranularity = Lens.field @"granularity"
{-# INLINEABLE emcGranularity #-}
{-# DEPRECATED granularity "Use generic-lens or generic-optics with 'granularity' instead"  #-}

-- | Specifies which group-level metrics to start collecting. You can specify one or more of the following metrics:
--
--
--     * @GroupMinSize@ 
--
--
--     * @GroupMaxSize@ 
--
--
--     * @GroupDesiredCapacity@ 
--
--
--     * @GroupInServiceInstances@ 
--
--
--     * @GroupPendingInstances@ 
--
--
--     * @GroupStandbyInstances@ 
--
--
--     * @GroupTerminatingInstances@ 
--
--
--     * @GroupTotalInstances@ 
--
--
-- The instance weighting feature supports the following additional metrics: 
--
--     * @GroupInServiceCapacity@ 
--
--
--     * @GroupPendingCapacity@ 
--
--
--     * @GroupStandbyCapacity@ 
--
--
--     * @GroupTerminatingCapacity@ 
--
--
--     * @GroupTotalCapacity@ 
--
--
-- If you omit this parameter, all metrics are enabled. 
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emcMetrics :: Lens.Lens' EnableMetricsCollection (Core.Maybe [Types.XmlStringMaxLen255])
emcMetrics = Lens.field @"metrics"
{-# INLINEABLE emcMetrics #-}
{-# DEPRECATED metrics "Use generic-lens or generic-optics with 'metrics' instead"  #-}

instance Core.ToQuery EnableMetricsCollection where
        toQuery EnableMetricsCollection{..}
          = Core.toQueryPair "Action"
              ("EnableMetricsCollection" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2011-01-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "AutoScalingGroupName" autoScalingGroupName
              Core.<> Core.toQueryPair "Granularity" granularity
              Core.<>
              Core.toQueryPair "Metrics"
                (Core.maybe Core.mempty (Core.toQueryList "member") metrics)

instance Core.ToHeaders EnableMetricsCollection where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest EnableMetricsCollection where
        type Rs EnableMetricsCollection = EnableMetricsCollectionResponse
        toRequest x@Core.Request{..}
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
          = Response.receiveNull EnableMetricsCollectionResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkEnableMetricsCollectionResponse' smart constructor.
data EnableMetricsCollectionResponse = EnableMetricsCollectionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableMetricsCollectionResponse' value with any optional fields omitted.
mkEnableMetricsCollectionResponse
    :: EnableMetricsCollectionResponse
mkEnableMetricsCollectionResponse
  = EnableMetricsCollectionResponse'
