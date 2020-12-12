{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.AutoScalingGroupProviderUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.AutoScalingGroupProviderUpdate
  ( AutoScalingGroupProviderUpdate (..),

    -- * Smart constructor
    mkAutoScalingGroupProviderUpdate,

    -- * Lenses
    asgpuManagedScaling,
    asgpuManagedTerminationProtection,
  )
where

import Network.AWS.ECS.Types.ManagedScaling
import Network.AWS.ECS.Types.ManagedTerminationProtection
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The details of the Auto Scaling group capacity provider to update.
--
-- /See:/ 'mkAutoScalingGroupProviderUpdate' smart constructor.
data AutoScalingGroupProviderUpdate = AutoScalingGroupProviderUpdate'
  { managedScaling ::
      Lude.Maybe ManagedScaling,
    managedTerminationProtection ::
      Lude.Maybe
        ManagedTerminationProtection
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AutoScalingGroupProviderUpdate' with the minimum fields required to make a request.
--
-- * 'managedScaling' - Undocumented field.
-- * 'managedTerminationProtection' - The managed termination protection setting to use for the Auto Scaling group capacity provider. This determines whether the Auto Scaling group has managed termination protection.
--
-- /Important:/ When using managed termination protection, managed scaling must also be used otherwise managed termination protection will not work.
-- When managed termination protection is enabled, Amazon ECS prevents the Amazon EC2 instances in an Auto Scaling group that contain tasks from being terminated during a scale-in action. The Auto Scaling group and each instance in the Auto Scaling group must have instance protection from scale-in actions enabled as well. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance Protection> in the /AWS Auto Scaling User Guide/ .
-- When managed termination protection is disabled, your Amazon EC2 instances are not protected from termination when the Auto Scaling group scales in.
mkAutoScalingGroupProviderUpdate ::
  AutoScalingGroupProviderUpdate
mkAutoScalingGroupProviderUpdate =
  AutoScalingGroupProviderUpdate'
    { managedScaling = Lude.Nothing,
      managedTerminationProtection = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'managedScaling' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgpuManagedScaling :: Lens.Lens' AutoScalingGroupProviderUpdate (Lude.Maybe ManagedScaling)
asgpuManagedScaling = Lens.lens (managedScaling :: AutoScalingGroupProviderUpdate -> Lude.Maybe ManagedScaling) (\s a -> s {managedScaling = a} :: AutoScalingGroupProviderUpdate)
{-# DEPRECATED asgpuManagedScaling "Use generic-lens or generic-optics with 'managedScaling' instead." #-}

-- | The managed termination protection setting to use for the Auto Scaling group capacity provider. This determines whether the Auto Scaling group has managed termination protection.
--
-- /Important:/ When using managed termination protection, managed scaling must also be used otherwise managed termination protection will not work.
-- When managed termination protection is enabled, Amazon ECS prevents the Amazon EC2 instances in an Auto Scaling group that contain tasks from being terminated during a scale-in action. The Auto Scaling group and each instance in the Auto Scaling group must have instance protection from scale-in actions enabled as well. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance Protection> in the /AWS Auto Scaling User Guide/ .
-- When managed termination protection is disabled, your Amazon EC2 instances are not protected from termination when the Auto Scaling group scales in.
--
-- /Note:/ Consider using 'managedTerminationProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgpuManagedTerminationProtection :: Lens.Lens' AutoScalingGroupProviderUpdate (Lude.Maybe ManagedTerminationProtection)
asgpuManagedTerminationProtection = Lens.lens (managedTerminationProtection :: AutoScalingGroupProviderUpdate -> Lude.Maybe ManagedTerminationProtection) (\s a -> s {managedTerminationProtection = a} :: AutoScalingGroupProviderUpdate)
{-# DEPRECATED asgpuManagedTerminationProtection "Use generic-lens or generic-optics with 'managedTerminationProtection' instead." #-}

instance Lude.ToJSON AutoScalingGroupProviderUpdate where
  toJSON AutoScalingGroupProviderUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("managedScaling" Lude..=) Lude.<$> managedScaling,
            ("managedTerminationProtection" Lude..=)
              Lude.<$> managedTerminationProtection
          ]
      )
