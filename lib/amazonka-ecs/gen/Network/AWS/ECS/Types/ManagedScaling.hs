{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.ManagedScaling
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.ManagedScaling
  ( ManagedScaling (..),

    -- * Smart constructor
    mkManagedScaling,

    -- * Lenses
    msInstanceWarmupPeriod,
    msMaximumScalingStepSize,
    msMinimumScalingStepSize,
    msStatus,
    msTargetCapacity,
  )
where

import qualified Network.AWS.ECS.Types.ManagedScalingStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The managed scaling settings for the Auto Scaling group capacity provider.
--
-- When managed scaling is enabled, Amazon ECS manages the scale-in and scale-out actions of the Auto Scaling group. Amazon ECS manages a target tracking scaling policy using an Amazon ECS-managed CloudWatch metric with the specified @targetCapacity@ value as the target value for the metric. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/asg-capacity-providers.html#asg-capacity-providers-managed-scaling Using Managed Scaling> in the /Amazon Elastic Container Service Developer Guide/ .
-- If managed scaling is disabled, the user must manage the scaling of the Auto Scaling group.
--
-- /See:/ 'mkManagedScaling' smart constructor.
data ManagedScaling = ManagedScaling'
  { -- | The period of time, in seconds, after a newly launched Amazon EC2 instance can contribute to CloudWatch metrics for Auto Scaling group. If this parameter is omitted, the default value of @300@ seconds is used.
    instanceWarmupPeriod :: Core.Maybe Core.Natural,
    -- | The maximum number of container instances that Amazon ECS will scale in or scale out at one time. If this parameter is omitted, the default value of @10000@ is used.
    maximumScalingStepSize :: Core.Maybe Core.Natural,
    -- | The minimum number of container instances that Amazon ECS will scale in or scale out at one time. If this parameter is omitted, the default value of @1@ is used.
    minimumScalingStepSize :: Core.Maybe Core.Natural,
    -- | Whether or not to enable managed scaling for the capacity provider.
    status :: Core.Maybe Types.ManagedScalingStatus,
    -- | The target capacity value for the capacity provider. The specified value must be greater than @0@ and less than or equal to @100@ . A value of @100@ will result in the Amazon EC2 instances in your Auto Scaling group being completely utilized.
    targetCapacity :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ManagedScaling' value with any optional fields omitted.
mkManagedScaling ::
  ManagedScaling
mkManagedScaling =
  ManagedScaling'
    { instanceWarmupPeriod = Core.Nothing,
      maximumScalingStepSize = Core.Nothing,
      minimumScalingStepSize = Core.Nothing,
      status = Core.Nothing,
      targetCapacity = Core.Nothing
    }

-- | The period of time, in seconds, after a newly launched Amazon EC2 instance can contribute to CloudWatch metrics for Auto Scaling group. If this parameter is omitted, the default value of @300@ seconds is used.
--
-- /Note:/ Consider using 'instanceWarmupPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msInstanceWarmupPeriod :: Lens.Lens' ManagedScaling (Core.Maybe Core.Natural)
msInstanceWarmupPeriod = Lens.field @"instanceWarmupPeriod"
{-# DEPRECATED msInstanceWarmupPeriod "Use generic-lens or generic-optics with 'instanceWarmupPeriod' instead." #-}

-- | The maximum number of container instances that Amazon ECS will scale in or scale out at one time. If this parameter is omitted, the default value of @10000@ is used.
--
-- /Note:/ Consider using 'maximumScalingStepSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msMaximumScalingStepSize :: Lens.Lens' ManagedScaling (Core.Maybe Core.Natural)
msMaximumScalingStepSize = Lens.field @"maximumScalingStepSize"
{-# DEPRECATED msMaximumScalingStepSize "Use generic-lens or generic-optics with 'maximumScalingStepSize' instead." #-}

-- | The minimum number of container instances that Amazon ECS will scale in or scale out at one time. If this parameter is omitted, the default value of @1@ is used.
--
-- /Note:/ Consider using 'minimumScalingStepSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msMinimumScalingStepSize :: Lens.Lens' ManagedScaling (Core.Maybe Core.Natural)
msMinimumScalingStepSize = Lens.field @"minimumScalingStepSize"
{-# DEPRECATED msMinimumScalingStepSize "Use generic-lens or generic-optics with 'minimumScalingStepSize' instead." #-}

-- | Whether or not to enable managed scaling for the capacity provider.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msStatus :: Lens.Lens' ManagedScaling (Core.Maybe Types.ManagedScalingStatus)
msStatus = Lens.field @"status"
{-# DEPRECATED msStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The target capacity value for the capacity provider. The specified value must be greater than @0@ and less than or equal to @100@ . A value of @100@ will result in the Amazon EC2 instances in your Auto Scaling group being completely utilized.
--
-- /Note:/ Consider using 'targetCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msTargetCapacity :: Lens.Lens' ManagedScaling (Core.Maybe Core.Natural)
msTargetCapacity = Lens.field @"targetCapacity"
{-# DEPRECATED msTargetCapacity "Use generic-lens or generic-optics with 'targetCapacity' instead." #-}

instance Core.FromJSON ManagedScaling where
  toJSON ManagedScaling {..} =
    Core.object
      ( Core.catMaybes
          [ ("instanceWarmupPeriod" Core..=) Core.<$> instanceWarmupPeriod,
            ("maximumScalingStepSize" Core..=) Core.<$> maximumScalingStepSize,
            ("minimumScalingStepSize" Core..=) Core.<$> minimumScalingStepSize,
            ("status" Core..=) Core.<$> status,
            ("targetCapacity" Core..=) Core.<$> targetCapacity
          ]
      )

instance Core.FromJSON ManagedScaling where
  parseJSON =
    Core.withObject "ManagedScaling" Core.$
      \x ->
        ManagedScaling'
          Core.<$> (x Core..:? "instanceWarmupPeriod")
          Core.<*> (x Core..:? "maximumScalingStepSize")
          Core.<*> (x Core..:? "minimumScalingStepSize")
          Core.<*> (x Core..:? "status")
          Core.<*> (x Core..:? "targetCapacity")
