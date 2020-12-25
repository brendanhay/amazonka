{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.DisableMetricsCollection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables group metrics for the specified Auto Scaling group.
module Network.AWS.AutoScaling.DisableMetricsCollection
  ( -- * Creating a request
    DisableMetricsCollection (..),
    mkDisableMetricsCollection,

    -- ** Request lenses
    dmcAutoScalingGroupName,
    dmcMetrics,

    -- * Destructuring the response
    DisableMetricsCollectionResponse (..),
    mkDisableMetricsCollectionResponse,
  )
where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisableMetricsCollection' smart constructor.
data DisableMetricsCollection = DisableMetricsCollection'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Types.AutoScalingGroupName,
    -- | Specifies one or more of the following metrics:
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
    -- If you omit this parameter, all metrics are disabled.
    metrics :: Core.Maybe [Types.XmlStringMaxLen255]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableMetricsCollection' value with any optional fields omitted.
mkDisableMetricsCollection ::
  -- | 'autoScalingGroupName'
  Types.AutoScalingGroupName ->
  DisableMetricsCollection
mkDisableMetricsCollection autoScalingGroupName =
  DisableMetricsCollection'
    { autoScalingGroupName,
      metrics = Core.Nothing
    }

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcAutoScalingGroupName :: Lens.Lens' DisableMetricsCollection Types.AutoScalingGroupName
dmcAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# DEPRECATED dmcAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | Specifies one or more of the following metrics:
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
-- If you omit this parameter, all metrics are disabled.
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcMetrics :: Lens.Lens' DisableMetricsCollection (Core.Maybe [Types.XmlStringMaxLen255])
dmcMetrics = Lens.field @"metrics"
{-# DEPRECATED dmcMetrics "Use generic-lens or generic-optics with 'metrics' instead." #-}

instance Core.AWSRequest DisableMetricsCollection where
  type Rs DisableMetricsCollection = DisableMetricsCollectionResponse
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
            ( Core.pure ("Action", "DisableMetricsCollection")
                Core.<> (Core.pure ("Version", "2011-01-01"))
                Core.<> (Core.toQueryValue "AutoScalingGroupName" autoScalingGroupName)
                Core.<> ( Core.toQueryValue
                            "Metrics"
                            (Core.toQueryList "member" Core.<$> metrics)
                        )
            )
      }
  response = Response.receiveNull DisableMetricsCollectionResponse'

-- | /See:/ 'mkDisableMetricsCollectionResponse' smart constructor.
data DisableMetricsCollectionResponse = DisableMetricsCollectionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableMetricsCollectionResponse' value with any optional fields omitted.
mkDisableMetricsCollectionResponse ::
  DisableMetricsCollectionResponse
mkDisableMetricsCollectionResponse =
  DisableMetricsCollectionResponse'
