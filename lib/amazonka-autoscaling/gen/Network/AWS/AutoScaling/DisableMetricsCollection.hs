{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DisableMetricsCollection (..)
    , mkDisableMetricsCollection
    -- ** Request lenses
    , dmcAutoScalingGroupName
    , dmcMetrics

    -- * Destructuring the response
    , DisableMetricsCollectionResponse (..)
    , mkDisableMetricsCollectionResponse
    ) where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDisableMetricsCollection' smart constructor.
data DisableMetricsCollection = DisableMetricsCollection'
  { autoScalingGroupName :: Types.AutoScalingGroupName
    -- ^ The name of the Auto Scaling group.
  , metrics :: Core.Maybe [Types.XmlStringMaxLen255]
    -- ^ Specifies one or more of the following metrics:
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableMetricsCollection' value with any optional fields omitted.
mkDisableMetricsCollection
    :: Types.AutoScalingGroupName -- ^ 'autoScalingGroupName'
    -> DisableMetricsCollection
mkDisableMetricsCollection autoScalingGroupName
  = DisableMetricsCollection'{autoScalingGroupName,
                              metrics = Core.Nothing}

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmcAutoScalingGroupName :: Lens.Lens' DisableMetricsCollection Types.AutoScalingGroupName
dmcAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# INLINEABLE dmcAutoScalingGroupName #-}
{-# DEPRECATED autoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead"  #-}

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
{-# INLINEABLE dmcMetrics #-}
{-# DEPRECATED metrics "Use generic-lens or generic-optics with 'metrics' instead"  #-}

instance Core.ToQuery DisableMetricsCollection where
        toQuery DisableMetricsCollection{..}
          = Core.toQueryPair "Action"
              ("DisableMetricsCollection" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2011-01-01" :: Core.Text)
              Core.<>
              Core.toQueryPair "AutoScalingGroupName" autoScalingGroupName
              Core.<>
              Core.toQueryPair "Metrics"
                (Core.maybe Core.mempty (Core.toQueryList "member") metrics)

instance Core.ToHeaders DisableMetricsCollection where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest DisableMetricsCollection where
        type Rs DisableMetricsCollection = DisableMetricsCollectionResponse
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
          = Response.receiveNull DisableMetricsCollectionResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDisableMetricsCollectionResponse' smart constructor.
data DisableMetricsCollectionResponse = DisableMetricsCollectionResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DisableMetricsCollectionResponse' value with any optional fields omitted.
mkDisableMetricsCollectionResponse
    :: DisableMetricsCollectionResponse
mkDisableMetricsCollectionResponse
  = DisableMetricsCollectionResponse'
