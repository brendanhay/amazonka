{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScaling.SetDesiredCapacity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the size of the specified Auto Scaling group.
--
-- If a scale-in activity occurs as a result of a new @DesiredCapacity@ value that is lower than the current size of the group, the Auto Scaling group uses its termination policy to determine which instances to terminate.
-- For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-manual-scaling.html Manual scaling> in the /Amazon EC2 Auto Scaling User Guide/ .
module Network.AWS.AutoScaling.SetDesiredCapacity
  ( -- * Creating a request
    SetDesiredCapacity (..),
    mkSetDesiredCapacity,

    -- ** Request lenses
    sdcAutoScalingGroupName,
    sdcDesiredCapacity,
    sdcHonorCooldown,

    -- * Destructuring the response
    SetDesiredCapacityResponse (..),
    mkSetDesiredCapacityResponse,
  )
where

import qualified Network.AWS.AutoScaling.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkSetDesiredCapacity' smart constructor.
data SetDesiredCapacity = SetDesiredCapacity'
  { -- | The name of the Auto Scaling group.
    autoScalingGroupName :: Types.ResourceName,
    -- | The desired capacity is the initial capacity of the Auto Scaling group after this operation completes and the capacity it attempts to maintain.
    desiredCapacity :: Core.Int,
    -- | Indicates whether Amazon EC2 Auto Scaling waits for the cooldown period to complete before initiating a scaling activity to set your Auto Scaling group to its new capacity. By default, Amazon EC2 Auto Scaling does not honor the cooldown period during manual scaling activities.
    honorCooldown :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetDesiredCapacity' value with any optional fields omitted.
mkSetDesiredCapacity ::
  -- | 'autoScalingGroupName'
  Types.ResourceName ->
  -- | 'desiredCapacity'
  Core.Int ->
  SetDesiredCapacity
mkSetDesiredCapacity autoScalingGroupName desiredCapacity =
  SetDesiredCapacity'
    { autoScalingGroupName,
      desiredCapacity,
      honorCooldown = Core.Nothing
    }

-- | The name of the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcAutoScalingGroupName :: Lens.Lens' SetDesiredCapacity Types.ResourceName
sdcAutoScalingGroupName = Lens.field @"autoScalingGroupName"
{-# DEPRECATED sdcAutoScalingGroupName "Use generic-lens or generic-optics with 'autoScalingGroupName' instead." #-}

-- | The desired capacity is the initial capacity of the Auto Scaling group after this operation completes and the capacity it attempts to maintain.
--
-- /Note:/ Consider using 'desiredCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcDesiredCapacity :: Lens.Lens' SetDesiredCapacity Core.Int
sdcDesiredCapacity = Lens.field @"desiredCapacity"
{-# DEPRECATED sdcDesiredCapacity "Use generic-lens or generic-optics with 'desiredCapacity' instead." #-}

-- | Indicates whether Amazon EC2 Auto Scaling waits for the cooldown period to complete before initiating a scaling activity to set your Auto Scaling group to its new capacity. By default, Amazon EC2 Auto Scaling does not honor the cooldown period during manual scaling activities.
--
-- /Note:/ Consider using 'honorCooldown' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdcHonorCooldown :: Lens.Lens' SetDesiredCapacity (Core.Maybe Core.Bool)
sdcHonorCooldown = Lens.field @"honorCooldown"
{-# DEPRECATED sdcHonorCooldown "Use generic-lens or generic-optics with 'honorCooldown' instead." #-}

instance Core.AWSRequest SetDesiredCapacity where
  type Rs SetDesiredCapacity = SetDesiredCapacityResponse
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
            ( Core.pure ("Action", "SetDesiredCapacity")
                Core.<> (Core.pure ("Version", "2011-01-01"))
                Core.<> (Core.toQueryValue "AutoScalingGroupName" autoScalingGroupName)
                Core.<> (Core.toQueryValue "DesiredCapacity" desiredCapacity)
                Core.<> (Core.toQueryValue "HonorCooldown" Core.<$> honorCooldown)
            )
      }
  response = Response.receiveNull SetDesiredCapacityResponse'

-- | /See:/ 'mkSetDesiredCapacityResponse' smart constructor.
data SetDesiredCapacityResponse = SetDesiredCapacityResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetDesiredCapacityResponse' value with any optional fields omitted.
mkSetDesiredCapacityResponse ::
  SetDesiredCapacityResponse
mkSetDesiredCapacityResponse = SetDesiredCapacityResponse'
