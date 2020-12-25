{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    EnableMetricsCollection (..),
    mkEnableMetricsCollection,

    -- ** Request lenses
    emcAutoScalingGroupName,
    emcGranularity,
    emcMetrics,

    -- * Destructuring the response
    EnableMetricsCollectionResponse (..),
    mkEnableMetricsCollectionResponse,
  )
where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkEnableMetricsCollection' smart constructor.
data EnableMetricsCollection = EnableMetricsCollection'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Types.ResourceName,
    -- | The granularity to associate with the metrics to collect. The only valid value is @1Minute@ .
    granularity :: Types.XmlStringMaxLen255,
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
    metrics :: Core.Maybe [Types.XmlStringMaxLen255]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableMetricsCollection' value with any optional fields omitted.
mkEnableMetricsCollection ::
  -- | 'autoScalingGroupName'
  Types.ResourceName ->
  -- | 'granularity'
  Types.XmlStringMaxLen255 ->
  EnableMetricsCollection
mkEnableMetricsCollection autoScalingGroupName granularity =
  EnableMetricsCollection'
    { autoScalingGroupName,
      granularity,
      metrics = Core.Nothing
    }

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emcAutoScalingGroupName :: Lens.Lens' EnableMetricsCollection Types.ResourceName
emcAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# DEPRECATED emcAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | The granularity to associate with the metrics to collect. The only valid value is @1Minute@ .
--
-- /Note:/ Consider using 'granularity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
emcGranularity :: Lens.Lens' EnableMetricsCollection Types.XmlStringMaxLen255
emcGranularity = Lens.field @"granularity"
{-# DEPRECATED emcGranularity "Use generic-lens or generic-optics with 'granularity' instead." #-}

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
{-# DEPRECATED emcMetrics "Use generic-lens or generic-optics with 'metrics' instead." #-}

instance Core.AWSRequest EnableMetricsCollection where
  type Rs EnableMetricsCollection = EnableMetricsCollectionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "EnableMetricsCollection")
                Core.<> (Core.pure ("Version", "2011-01-01"))
                Core.<> (Core.toQueryValue "AutoScalingGroupName" autoScalingGroupName)
                Core.<> (Core.toQueryValue "Granularity" granularity)
                Core.<> ( Core.toQueryValue
                            "Metrics"
                            (Core.toQueryList "member" Core.<$> metrics)
                        )
            )
      }
  response = Response.receiveNull EnableMetricsCollectionResponse'

-- | /See:/ 'mkEnableMetricsCollectionResponse' smart constructor.
data EnableMetricsCollectionResponse = EnableMetricsCollectionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EnableMetricsCollectionResponse' value with any optional fields omitted.
mkEnableMetricsCollectionResponse ::
  EnableMetricsCollectionResponse
mkEnableMetricsCollectionResponse =
  EnableMetricsCollectionResponse'
