{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.AutoScalingGroupProviderUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.AutoScalingGroupProviderUpdate
  ( AutoScalingGroupProviderUpdate (..)
  -- * Smart constructor
  , mkAutoScalingGroupProviderUpdate
  -- * Lenses
  , asgpuManagedScaling
  , asgpuManagedTerminationProtection
  ) where

import qualified Network.AWS.ECS.Types.ManagedScaling as Types
import qualified Network.AWS.ECS.Types.ManagedTerminationProtection as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The details of the Auto Scaling group capacity provider to update.
--
-- /See:/ 'mkAutoScalingGroupProviderUpdate' smart constructor.
data AutoScalingGroupProviderUpdate = AutoScalingGroupProviderUpdate'
  { managedScaling :: Core.Maybe Types.ManagedScaling
  , managedTerminationProtection :: Core.Maybe Types.ManagedTerminationProtection
    -- ^ The managed termination protection setting to use for the Auto Scaling group capacity provider. This determines whether the Auto Scaling group has managed termination protection.
--
-- /Important:/ When using managed termination protection, managed scaling must also be used otherwise managed termination protection will not work.
-- When managed termination protection is enabled, Amazon ECS prevents the Amazon EC2 instances in an Auto Scaling group that contain tasks from being terminated during a scale-in action. The Auto Scaling group and each instance in the Auto Scaling group must have instance protection from scale-in actions enabled as well. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance Protection> in the /AWS Auto Scaling User Guide/ .
-- When managed termination protection is disabled, your Amazon EC2 instances are not protected from termination when the Auto Scaling group scales in.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AutoScalingGroupProviderUpdate' value with any optional fields omitted.
mkAutoScalingGroupProviderUpdate
    :: AutoScalingGroupProviderUpdate
mkAutoScalingGroupProviderUpdate
  = AutoScalingGroupProviderUpdate'{managedScaling = Core.Nothing,
                                    managedTerminationProtection = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'managedScaling' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgpuManagedScaling :: Lens.Lens' AutoScalingGroupProviderUpdate (Core.Maybe Types.ManagedScaling)
asgpuManagedScaling = Lens.field @"managedScaling"
{-# INLINEABLE asgpuManagedScaling #-}
{-# DEPRECATED managedScaling "Use generic-lens or generic-optics with 'managedScaling' instead"  #-}

-- | The managed termination protection setting to use for the Auto Scaling group capacity provider. This determines whether the Auto Scaling group has managed termination protection.
--
-- /Important:/ When using managed termination protection, managed scaling must also be used otherwise managed termination protection will not work.
-- When managed termination protection is enabled, Amazon ECS prevents the Amazon EC2 instances in an Auto Scaling group that contain tasks from being terminated during a scale-in action. The Auto Scaling group and each instance in the Auto Scaling group must have instance protection from scale-in actions enabled as well. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance Protection> in the /AWS Auto Scaling User Guide/ .
-- When managed termination protection is disabled, your Amazon EC2 instances are not protected from termination when the Auto Scaling group scales in.
--
-- /Note:/ Consider using 'managedTerminationProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgpuManagedTerminationProtection :: Lens.Lens' AutoScalingGroupProviderUpdate (Core.Maybe Types.ManagedTerminationProtection)
asgpuManagedTerminationProtection = Lens.field @"managedTerminationProtection"
{-# INLINEABLE asgpuManagedTerminationProtection #-}
{-# DEPRECATED managedTerminationProtection "Use generic-lens or generic-optics with 'managedTerminationProtection' instead"  #-}

instance Core.FromJSON AutoScalingGroupProviderUpdate where
        toJSON AutoScalingGroupProviderUpdate{..}
          = Core.object
              (Core.catMaybes
                 [("managedScaling" Core..=) Core.<$> managedScaling,
                  ("managedTerminationProtection" Core..=) Core.<$>
                    managedTerminationProtection])
