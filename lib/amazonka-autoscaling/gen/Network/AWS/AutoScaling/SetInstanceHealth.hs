{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.SetInstanceHealth
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the health status of the specified instance.
--
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/healthcheck.html Health checks for Auto Scaling instances> in the /Amazon EC2 Auto Scaling User Guide/ .
module Network.AWS.AutoScaling.SetInstanceHealth
    (
    -- * Creating a request
      SetInstanceHealth (..)
    , mkSetInstanceHealth
    -- ** Request lenses
    , sihInstanceId
    , sihHealthStatus
    , sihShouldRespectGracePeriod

    -- * Destructuring the response
    , SetInstanceHealthResponse (..)
    , mkSetInstanceHealthResponse
    ) where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSetInstanceHealth' smart constructor.
data SetInstanceHealth = SetInstanceHealth'
  { instanceId :: Types.InstanceId
    -- ^ The ID of the instance.
  , healthStatus :: Types.HealthStatus
    -- ^ The health status of the instance. Set to @Healthy@ to have the instance remain in service. Set to @Unhealthy@ to have the instance be out of service. Amazon EC2 Auto Scaling terminates and replaces the unhealthy instance.
  , shouldRespectGracePeriod :: Core.Maybe Core.Bool
    -- ^ If the Auto Scaling group of the specified instance has a @HealthCheckGracePeriod@ specified for the group, by default, this call respects the grace period. Set this to @False@ , to have the call not respect the grace period associated with the group.
--
-- For more information about the health check grace period, see <https://docs.aws.amazon.com/autoscaling/ec2/APIReference/API_CreateAutoScalingGroup.html CreateAutoScalingGroup> in the /Amazon EC2 Auto Scaling API Reference/ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetInstanceHealth' value with any optional fields omitted.
mkSetInstanceHealth
    :: Types.InstanceId -- ^ 'instanceId'
    -> Types.HealthStatus -- ^ 'healthStatus'
    -> SetInstanceHealth
mkSetInstanceHealth instanceId healthStatus
  = SetInstanceHealth'{instanceId, healthStatus,
                       shouldRespectGracePeriod = Core.Nothing}

-- | The ID of the instance.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sihInstanceId :: Lens.Lens' SetInstanceHealth Types.InstanceId
sihInstanceId = Lens.field @"instanceId"
{-# INLINEABLE sihInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The health status of the instance. Set to @Healthy@ to have the instance remain in service. Set to @Unhealthy@ to have the instance be out of service. Amazon EC2 Auto Scaling terminates and replaces the unhealthy instance.
--
-- /Note:/ Consider using 'healthStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sihHealthStatus :: Lens.Lens' SetInstanceHealth Types.HealthStatus
sihHealthStatus = Lens.field @"healthStatus"
{-# INLINEABLE sihHealthStatus #-}
{-# DEPRECATED healthStatus "Use generic-lens or generic-optics with 'healthStatus' instead"  #-}

-- | If the Auto Scaling group of the specified instance has a @HealthCheckGracePeriod@ specified for the group, by default, this call respects the grace period. Set this to @False@ , to have the call not respect the grace period associated with the group.
--
-- For more information about the health check grace period, see <https://docs.aws.amazon.com/autoscaling/ec2/APIReference/API_CreateAutoScalingGroup.html CreateAutoScalingGroup> in the /Amazon EC2 Auto Scaling API Reference/ .
--
-- /Note:/ Consider using 'shouldRespectGracePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sihShouldRespectGracePeriod :: Lens.Lens' SetInstanceHealth (Core.Maybe Core.Bool)
sihShouldRespectGracePeriod = Lens.field @"shouldRespectGracePeriod"
{-# INLINEABLE sihShouldRespectGracePeriod #-}
{-# DEPRECATED shouldRespectGracePeriod "Use generic-lens or generic-optics with 'shouldRespectGracePeriod' instead"  #-}

instance Core.ToQuery SetInstanceHealth where
        toQuery SetInstanceHealth{..}
          = Core.toQueryPair "Action" ("SetInstanceHealth" :: Core.Text)
              Core.<> Core.toQueryPair "Version" ("2011-01-01" :: Core.Text)
              Core.<> Core.toQueryPair "InstanceId" instanceId
              Core.<> Core.toQueryPair "HealthStatus" healthStatus
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "ShouldRespectGracePeriod")
                shouldRespectGracePeriod

instance Core.ToHeaders SetInstanceHealth where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest SetInstanceHealth where
        type Rs SetInstanceHealth = SetInstanceHealthResponse
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
        parseResponse = Response.receiveNull SetInstanceHealthResponse'
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkSetInstanceHealthResponse' smart constructor.
data SetInstanceHealthResponse = SetInstanceHealthResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetInstanceHealthResponse' value with any optional fields omitted.
mkSetInstanceHealthResponse
    :: SetInstanceHealthResponse
mkSetInstanceHealthResponse = SetInstanceHealthResponse'
