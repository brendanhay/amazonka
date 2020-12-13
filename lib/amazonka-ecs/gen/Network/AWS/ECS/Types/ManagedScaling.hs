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
    msStatus,
    msMaximumScalingStepSize,
    msTargetCapacity,
    msMinimumScalingStepSize,
    msInstanceWarmupPeriod,
  )
where

import Network.AWS.ECS.Types.ManagedScalingStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The managed scaling settings for the Auto Scaling group capacity provider.
--
-- When managed scaling is enabled, Amazon ECS manages the scale-in and scale-out actions of the Auto Scaling group. Amazon ECS manages a target tracking scaling policy using an Amazon ECS-managed CloudWatch metric with the specified @targetCapacity@ value as the target value for the metric. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/asg-capacity-providers.html#asg-capacity-providers-managed-scaling Using Managed Scaling> in the /Amazon Elastic Container Service Developer Guide/ .
-- If managed scaling is disabled, the user must manage the scaling of the Auto Scaling group.
--
-- /See:/ 'mkManagedScaling' smart constructor.
data ManagedScaling = ManagedScaling'
  { -- | Whether or not to enable managed scaling for the capacity provider.
    status :: Lude.Maybe ManagedScalingStatus,
    -- | The maximum number of container instances that Amazon ECS will scale in or scale out at one time. If this parameter is omitted, the default value of @10000@ is used.
    maximumScalingStepSize :: Lude.Maybe Lude.Natural,
    -- | The target capacity value for the capacity provider. The specified value must be greater than @0@ and less than or equal to @100@ . A value of @100@ will result in the Amazon EC2 instances in your Auto Scaling group being completely utilized.
    targetCapacity :: Lude.Maybe Lude.Natural,
    -- | The minimum number of container instances that Amazon ECS will scale in or scale out at one time. If this parameter is omitted, the default value of @1@ is used.
    minimumScalingStepSize :: Lude.Maybe Lude.Natural,
    -- | The period of time, in seconds, after a newly launched Amazon EC2 instance can contribute to CloudWatch metrics for Auto Scaling group. If this parameter is omitted, the default value of @300@ seconds is used.
    instanceWarmupPeriod :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ManagedScaling' with the minimum fields required to make a request.
--
-- * 'status' - Whether or not to enable managed scaling for the capacity provider.
-- * 'maximumScalingStepSize' - The maximum number of container instances that Amazon ECS will scale in or scale out at one time. If this parameter is omitted, the default value of @10000@ is used.
-- * 'targetCapacity' - The target capacity value for the capacity provider. The specified value must be greater than @0@ and less than or equal to @100@ . A value of @100@ will result in the Amazon EC2 instances in your Auto Scaling group being completely utilized.
-- * 'minimumScalingStepSize' - The minimum number of container instances that Amazon ECS will scale in or scale out at one time. If this parameter is omitted, the default value of @1@ is used.
-- * 'instanceWarmupPeriod' - The period of time, in seconds, after a newly launched Amazon EC2 instance can contribute to CloudWatch metrics for Auto Scaling group. If this parameter is omitted, the default value of @300@ seconds is used.
mkManagedScaling ::
  ManagedScaling
mkManagedScaling =
  ManagedScaling'
    { status = Lude.Nothing,
      maximumScalingStepSize = Lude.Nothing,
      targetCapacity = Lude.Nothing,
      minimumScalingStepSize = Lude.Nothing,
      instanceWarmupPeriod = Lude.Nothing
    }

-- | Whether or not to enable managed scaling for the capacity provider.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msStatus :: Lens.Lens' ManagedScaling (Lude.Maybe ManagedScalingStatus)
msStatus = Lens.lens (status :: ManagedScaling -> Lude.Maybe ManagedScalingStatus) (\s a -> s {status = a} :: ManagedScaling)
{-# DEPRECATED msStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The maximum number of container instances that Amazon ECS will scale in or scale out at one time. If this parameter is omitted, the default value of @10000@ is used.
--
-- /Note:/ Consider using 'maximumScalingStepSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msMaximumScalingStepSize :: Lens.Lens' ManagedScaling (Lude.Maybe Lude.Natural)
msMaximumScalingStepSize = Lens.lens (maximumScalingStepSize :: ManagedScaling -> Lude.Maybe Lude.Natural) (\s a -> s {maximumScalingStepSize = a} :: ManagedScaling)
{-# DEPRECATED msMaximumScalingStepSize "Use generic-lens or generic-optics with 'maximumScalingStepSize' instead." #-}

-- | The target capacity value for the capacity provider. The specified value must be greater than @0@ and less than or equal to @100@ . A value of @100@ will result in the Amazon EC2 instances in your Auto Scaling group being completely utilized.
--
-- /Note:/ Consider using 'targetCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msTargetCapacity :: Lens.Lens' ManagedScaling (Lude.Maybe Lude.Natural)
msTargetCapacity = Lens.lens (targetCapacity :: ManagedScaling -> Lude.Maybe Lude.Natural) (\s a -> s {targetCapacity = a} :: ManagedScaling)
{-# DEPRECATED msTargetCapacity "Use generic-lens or generic-optics with 'targetCapacity' instead." #-}

-- | The minimum number of container instances that Amazon ECS will scale in or scale out at one time. If this parameter is omitted, the default value of @1@ is used.
--
-- /Note:/ Consider using 'minimumScalingStepSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msMinimumScalingStepSize :: Lens.Lens' ManagedScaling (Lude.Maybe Lude.Natural)
msMinimumScalingStepSize = Lens.lens (minimumScalingStepSize :: ManagedScaling -> Lude.Maybe Lude.Natural) (\s a -> s {minimumScalingStepSize = a} :: ManagedScaling)
{-# DEPRECATED msMinimumScalingStepSize "Use generic-lens or generic-optics with 'minimumScalingStepSize' instead." #-}

-- | The period of time, in seconds, after a newly launched Amazon EC2 instance can contribute to CloudWatch metrics for Auto Scaling group. If this parameter is omitted, the default value of @300@ seconds is used.
--
-- /Note:/ Consider using 'instanceWarmupPeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msInstanceWarmupPeriod :: Lens.Lens' ManagedScaling (Lude.Maybe Lude.Natural)
msInstanceWarmupPeriod = Lens.lens (instanceWarmupPeriod :: ManagedScaling -> Lude.Maybe Lude.Natural) (\s a -> s {instanceWarmupPeriod = a} :: ManagedScaling)
{-# DEPRECATED msInstanceWarmupPeriod "Use generic-lens or generic-optics with 'instanceWarmupPeriod' instead." #-}

instance Lude.FromJSON ManagedScaling where
  parseJSON =
    Lude.withObject
      "ManagedScaling"
      ( \x ->
          ManagedScaling'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "maximumScalingStepSize")
            Lude.<*> (x Lude..:? "targetCapacity")
            Lude.<*> (x Lude..:? "minimumScalingStepSize")
            Lude.<*> (x Lude..:? "instanceWarmupPeriod")
      )

instance Lude.ToJSON ManagedScaling where
  toJSON ManagedScaling' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("status" Lude..=) Lude.<$> status,
            ("maximumScalingStepSize" Lude..=) Lude.<$> maximumScalingStepSize,
            ("targetCapacity" Lude..=) Lude.<$> targetCapacity,
            ("minimumScalingStepSize" Lude..=) Lude.<$> minimumScalingStepSize,
            ("instanceWarmupPeriod" Lude..=) Lude.<$> instanceWarmupPeriod
          ]
      )
