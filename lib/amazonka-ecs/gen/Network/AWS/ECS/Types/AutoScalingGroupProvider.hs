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
    asgpManagedScaling,
    asgpManagedTerminationProtection,
    asgpAutoScalingGroupARN,
  )
where

import Network.AWS.ECS.Types.ManagedScaling
import Network.AWS.ECS.Types.ManagedTerminationProtection
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The details of the Auto Scaling group for the capacity provider.
--
-- /See:/ 'mkAutoScalingGroupProvider' smart constructor.
data AutoScalingGroupProvider = AutoScalingGroupProvider'
  { -- | The managed scaling settings for the Auto Scaling group capacity provider.
    managedScaling :: Lude.Maybe ManagedScaling,
    -- | The managed termination protection setting to use for the Auto Scaling group capacity provider. This determines whether the Auto Scaling group has managed termination protection.
    --
    -- /Important:/ When using managed termination protection, managed scaling must also be used otherwise managed termination protection will not work.
    -- When managed termination protection is enabled, Amazon ECS prevents the Amazon EC2 instances in an Auto Scaling group that contain tasks from being terminated during a scale-in action. The Auto Scaling group and each instance in the Auto Scaling group must have instance protection from scale-in actions enabled as well. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance Protection> in the /AWS Auto Scaling User Guide/ .
    -- When managed termination protection is disabled, your Amazon EC2 instances are not protected from termination when the Auto Scaling group scales in.
    managedTerminationProtection :: Lude.Maybe ManagedTerminationProtection,
    -- | The Amazon Resource Name (ARN) that identifies the Auto Scaling group.
    autoScalingGroupARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AutoScalingGroupProvider' with the minimum fields required to make a request.
--
-- * 'managedScaling' - The managed scaling settings for the Auto Scaling group capacity provider.
-- * 'managedTerminationProtection' - The managed termination protection setting to use for the Auto Scaling group capacity provider. This determines whether the Auto Scaling group has managed termination protection.
--
-- /Important:/ When using managed termination protection, managed scaling must also be used otherwise managed termination protection will not work.
-- When managed termination protection is enabled, Amazon ECS prevents the Amazon EC2 instances in an Auto Scaling group that contain tasks from being terminated during a scale-in action. The Auto Scaling group and each instance in the Auto Scaling group must have instance protection from scale-in actions enabled as well. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance Protection> in the /AWS Auto Scaling User Guide/ .
-- When managed termination protection is disabled, your Amazon EC2 instances are not protected from termination when the Auto Scaling group scales in.
-- * 'autoScalingGroupARN' - The Amazon Resource Name (ARN) that identifies the Auto Scaling group.
mkAutoScalingGroupProvider ::
  -- | 'autoScalingGroupARN'
  Lude.Text ->
  AutoScalingGroupProvider
mkAutoScalingGroupProvider pAutoScalingGroupARN_ =
  AutoScalingGroupProvider'
    { managedScaling = Lude.Nothing,
      managedTerminationProtection = Lude.Nothing,
      autoScalingGroupARN = pAutoScalingGroupARN_
    }

-- | The managed scaling settings for the Auto Scaling group capacity provider.
--
-- /Note:/ Consider using 'managedScaling' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgpManagedScaling :: Lens.Lens' AutoScalingGroupProvider (Lude.Maybe ManagedScaling)
asgpManagedScaling = Lens.lens (managedScaling :: AutoScalingGroupProvider -> Lude.Maybe ManagedScaling) (\s a -> s {managedScaling = a} :: AutoScalingGroupProvider)
{-# DEPRECATED asgpManagedScaling "Use generic-lens or generic-optics with 'managedScaling' instead." #-}

-- | The managed termination protection setting to use for the Auto Scaling group capacity provider. This determines whether the Auto Scaling group has managed termination protection.
--
-- /Important:/ When using managed termination protection, managed scaling must also be used otherwise managed termination protection will not work.
-- When managed termination protection is enabled, Amazon ECS prevents the Amazon EC2 instances in an Auto Scaling group that contain tasks from being terminated during a scale-in action. The Auto Scaling group and each instance in the Auto Scaling group must have instance protection from scale-in actions enabled as well. For more information, see <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance Protection> in the /AWS Auto Scaling User Guide/ .
-- When managed termination protection is disabled, your Amazon EC2 instances are not protected from termination when the Auto Scaling group scales in.
--
-- /Note:/ Consider using 'managedTerminationProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgpManagedTerminationProtection :: Lens.Lens' AutoScalingGroupProvider (Lude.Maybe ManagedTerminationProtection)
asgpManagedTerminationProtection = Lens.lens (managedTerminationProtection :: AutoScalingGroupProvider -> Lude.Maybe ManagedTerminationProtection) (\s a -> s {managedTerminationProtection = a} :: AutoScalingGroupProvider)
{-# DEPRECATED asgpManagedTerminationProtection "Use generic-lens or generic-optics with 'managedTerminationProtection' instead." #-}

-- | The Amazon Resource Name (ARN) that identifies the Auto Scaling group.
--
-- /Note:/ Consider using 'autoScalingGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asgpAutoScalingGroupARN :: Lens.Lens' AutoScalingGroupProvider Lude.Text
asgpAutoScalingGroupARN = Lens.lens (autoScalingGroupARN :: AutoScalingGroupProvider -> Lude.Text) (\s a -> s {autoScalingGroupARN = a} :: AutoScalingGroupProvider)
{-# DEPRECATED asgpAutoScalingGroupARN "Use generic-lens or generic-optics with 'autoScalingGroupARN' instead." #-}

instance Lude.FromJSON AutoScalingGroupProvider where
  parseJSON =
    Lude.withObject
      "AutoScalingGroupProvider"
      ( \x ->
          AutoScalingGroupProvider'
            Lude.<$> (x Lude..:? "managedScaling")
            Lude.<*> (x Lude..:? "managedTerminationProtection")
            Lude.<*> (x Lude..: "autoScalingGroupArn")
      )

instance Lude.ToJSON AutoScalingGroupProvider where
  toJSON AutoScalingGroupProvider' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("managedScaling" Lude..=) Lude.<$> managedScaling,
            ("managedTerminationProtection" Lude..=)
              Lude.<$> managedTerminationProtection,
            Lude.Just ("autoScalingGroupArn" Lude..= autoScalingGroupARN)
          ]
      )
