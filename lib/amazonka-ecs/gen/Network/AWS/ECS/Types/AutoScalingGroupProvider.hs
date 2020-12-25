{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.AutoScalingGroupProvider
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.AutoScalingGroupProvider
  ( AutoScalingGroupProvider (..),

    -- * Smart constructor
    mkAutoScalingGroupProvider,

    -- * Lenses
    asgpAutoScalingGroupArn,
    asgpManagedScaling,
    asgpManagedTerminationProtection,
  )
where

import qualified Network.AWS.ECS.Types.ManagedScaling as Types
import qualified Network.AWS.ECS.Types.ManagedTerminationProtection as Types
import qualified Network.AWS.ECS.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The details of the Auto Scaling group for the capacity provider.
--
-- /See:/ 'mkAutoScalingGroupProvider' smart constructor.
data AutoScalingGroupProvider = AutoScalingGroupProvider'
  { -- | The Amazon Resource Name (ARN) that identifies the Auto Scaling group.
    autoScalingGroupArn :: Types.String,
    -- | The managed scaling settings for the Auto Scaling group capacity provider.
    managedScaling :: Core.Maybe Types.ManagedScaling,
    -- | The managed termination protection setting to use for the Auto Scaling group capacity provider. This determines whether the Auto Scaling group has managed termination protection.
    --
    -- /Important:/ When using managed termination protection, managed scaling must also be used otherwise managed termination protection will not work.
    -- When managed termination protection is enabled, Amazon ECS prevents the Amazon EC2 instances in an Auto Scaling group that contain tasks from being terminated during a scale-in action. The Auto Scaling group and each instance in the Auto Scaling group must have instance protection from scale-in actions enabled as well. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance Protection> in the /AWS Auto Scaling User Guide/ .
    -- When managed termination protection is disabled, your Amazon EC2 instances are not protected from termination when the Auto Scaling group scales in.
    managedTerminationProtection :: Core.Maybe Types.ManagedTerminationProtection
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AutoScalingGroupProvider' value with any optional fields omitted.
mkAutoScalingGroupProvider ::
  -- | 'autoScalingGroupArn'
  Types.String ->
  AutoScalingGroupProvider
mkAutoScalingGroupProvider autoScalingGroupArn =
  AutoScalingGroupProvider'
    { autoScalingGroupArn,
      managedScaling = Core.Nothing,
      managedTerminationProtection = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) that identifies the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgpAutoScalingGroupArn :: Lens.Lens' AutoScalingGroupProvider Types.String
asgpAutoScalingGroupArn = Lens.field @"autoScalingGroupArn"
{-# DEPRECATED asgpAutoScalingGroupArn "Use generic-lens or generic-optics with 'autoScalingGroupArn' instead." #-}

-- | The managed scaling settings for the Auto Scaling group capacity provider.
--
-- /Note:/ Consider using 'managedScaling' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgpManagedScaling :: Lens.Lens' AutoScalingGroupProvider (Core.Maybe Types.ManagedScaling)
asgpManagedScaling = Lens.field @"managedScaling"
{-# DEPRECATED asgpManagedScaling "Use generic-lens or generic-optics with 'managedScaling' instead." #-}

-- | The managed termination protection setting to use for the Auto Scaling group capacity provider. This determines whether the Auto Scaling group has managed termination protection.
--
-- /Important:/ When using managed termination protection, managed scaling must also be used otherwise managed termination protection will not work.
-- When managed termination protection is enabled, Amazon ECS prevents the Amazon EC2 instances in an Auto Scaling group that contain tasks from being terminated during a scale-in action. The Auto Scaling group and each instance in the Auto Scaling group must have instance protection from scale-in actions enabled as well. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance Protection> in the /AWS Auto Scaling User Guide/ .
-- When managed termination protection is disabled, your Amazon EC2 instances are not protected from termination when the Auto Scaling group scales in.
--
-- /Note:/ Consider using 'managedTerminationProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgpManagedTerminationProtection :: Lens.Lens' AutoScalingGroupProvider (Core.Maybe Types.ManagedTerminationProtection)
asgpManagedTerminationProtection = Lens.field @"managedTerminationProtection"
{-# DEPRECATED asgpManagedTerminationProtection "Use generic-lens or generic-optics with 'managedTerminationProtection' instead." #-}

instance Core.FromJSON AutoScalingGroupProvider where
  toJSON AutoScalingGroupProvider {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("autoScalingGroupArn" Core..= autoScalingGroupArn),
            ("managedScaling" Core..=) Core.<$> managedScaling,
            ("managedTerminationProtection" Core..=)
              Core.<$> managedTerminationProtection
          ]
      )

instance Core.FromJSON AutoScalingGroupProvider where
  parseJSON =
    Core.withObject "AutoScalingGroupProvider" Core.$
      \x ->
        AutoScalingGroupProvider'
          Core.<$> (x Core..: "autoScalingGroupArn")
          Core.<*> (x Core..:? "managedScaling")
          Core.<*> (x Core..:? "managedTerminationProtection")
