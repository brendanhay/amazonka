{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.AutoScalingGroupProvider
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.AutoScalingGroupProvider where

import Network.AWS.ECS.Types.ManagedScaling
import Network.AWS.ECS.Types.ManagedTerminationProtection
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The details of the Auto Scaling group for the capacity provider.
--
-- /See:/ 'newAutoScalingGroupProvider' smart constructor.
data AutoScalingGroupProvider = AutoScalingGroupProvider'
  { -- | The managed scaling settings for the Auto Scaling group capacity
    -- provider.
    managedScaling :: Prelude.Maybe ManagedScaling,
    -- | The managed termination protection setting to use for the Auto Scaling
    -- group capacity provider. This determines whether the Auto Scaling group
    -- has managed termination protection.
    --
    -- When using managed termination protection, managed scaling must also be
    -- used otherwise managed termination protection will not work.
    --
    -- When managed termination protection is enabled, Amazon ECS prevents the
    -- Amazon EC2 instances in an Auto Scaling group that contain tasks from
    -- being terminated during a scale-in action. The Auto Scaling group and
    -- each instance in the Auto Scaling group must have instance protection
    -- from scale-in actions enabled as well. For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance Protection>
    -- in the /AWS Auto Scaling User Guide/.
    --
    -- When managed termination protection is disabled, your Amazon EC2
    -- instances are not protected from termination when the Auto Scaling group
    -- scales in.
    managedTerminationProtection :: Prelude.Maybe ManagedTerminationProtection,
    -- | The Amazon Resource Name (ARN) that identifies the Auto Scaling group.
    autoScalingGroupArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AutoScalingGroupProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'managedScaling', 'autoScalingGroupProvider_managedScaling' - The managed scaling settings for the Auto Scaling group capacity
-- provider.
--
-- 'managedTerminationProtection', 'autoScalingGroupProvider_managedTerminationProtection' - The managed termination protection setting to use for the Auto Scaling
-- group capacity provider. This determines whether the Auto Scaling group
-- has managed termination protection.
--
-- When using managed termination protection, managed scaling must also be
-- used otherwise managed termination protection will not work.
--
-- When managed termination protection is enabled, Amazon ECS prevents the
-- Amazon EC2 instances in an Auto Scaling group that contain tasks from
-- being terminated during a scale-in action. The Auto Scaling group and
-- each instance in the Auto Scaling group must have instance protection
-- from scale-in actions enabled as well. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance Protection>
-- in the /AWS Auto Scaling User Guide/.
--
-- When managed termination protection is disabled, your Amazon EC2
-- instances are not protected from termination when the Auto Scaling group
-- scales in.
--
-- 'autoScalingGroupArn', 'autoScalingGroupProvider_autoScalingGroupArn' - The Amazon Resource Name (ARN) that identifies the Auto Scaling group.
newAutoScalingGroupProvider ::
  -- | 'autoScalingGroupArn'
  Prelude.Text ->
  AutoScalingGroupProvider
newAutoScalingGroupProvider pAutoScalingGroupArn_ =
  AutoScalingGroupProvider'
    { managedScaling =
        Prelude.Nothing,
      managedTerminationProtection = Prelude.Nothing,
      autoScalingGroupArn = pAutoScalingGroupArn_
    }

-- | The managed scaling settings for the Auto Scaling group capacity
-- provider.
autoScalingGroupProvider_managedScaling :: Lens.Lens' AutoScalingGroupProvider (Prelude.Maybe ManagedScaling)
autoScalingGroupProvider_managedScaling = Lens.lens (\AutoScalingGroupProvider' {managedScaling} -> managedScaling) (\s@AutoScalingGroupProvider' {} a -> s {managedScaling = a} :: AutoScalingGroupProvider)

-- | The managed termination protection setting to use for the Auto Scaling
-- group capacity provider. This determines whether the Auto Scaling group
-- has managed termination protection.
--
-- When using managed termination protection, managed scaling must also be
-- used otherwise managed termination protection will not work.
--
-- When managed termination protection is enabled, Amazon ECS prevents the
-- Amazon EC2 instances in an Auto Scaling group that contain tasks from
-- being terminated during a scale-in action. The Auto Scaling group and
-- each instance in the Auto Scaling group must have instance protection
-- from scale-in actions enabled as well. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance Protection>
-- in the /AWS Auto Scaling User Guide/.
--
-- When managed termination protection is disabled, your Amazon EC2
-- instances are not protected from termination when the Auto Scaling group
-- scales in.
autoScalingGroupProvider_managedTerminationProtection :: Lens.Lens' AutoScalingGroupProvider (Prelude.Maybe ManagedTerminationProtection)
autoScalingGroupProvider_managedTerminationProtection = Lens.lens (\AutoScalingGroupProvider' {managedTerminationProtection} -> managedTerminationProtection) (\s@AutoScalingGroupProvider' {} a -> s {managedTerminationProtection = a} :: AutoScalingGroupProvider)

-- | The Amazon Resource Name (ARN) that identifies the Auto Scaling group.
autoScalingGroupProvider_autoScalingGroupArn :: Lens.Lens' AutoScalingGroupProvider Prelude.Text
autoScalingGroupProvider_autoScalingGroupArn = Lens.lens (\AutoScalingGroupProvider' {autoScalingGroupArn} -> autoScalingGroupArn) (\s@AutoScalingGroupProvider' {} a -> s {autoScalingGroupArn = a} :: AutoScalingGroupProvider)

instance Prelude.FromJSON AutoScalingGroupProvider where
  parseJSON =
    Prelude.withObject
      "AutoScalingGroupProvider"
      ( \x ->
          AutoScalingGroupProvider'
            Prelude.<$> (x Prelude..:? "managedScaling")
            Prelude.<*> (x Prelude..:? "managedTerminationProtection")
            Prelude.<*> (x Prelude..: "autoScalingGroupArn")
      )

instance Prelude.Hashable AutoScalingGroupProvider

instance Prelude.NFData AutoScalingGroupProvider

instance Prelude.ToJSON AutoScalingGroupProvider where
  toJSON AutoScalingGroupProvider' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("managedScaling" Prelude..=)
              Prelude.<$> managedScaling,
            ("managedTerminationProtection" Prelude..=)
              Prelude.<$> managedTerminationProtection,
            Prelude.Just
              ( "autoScalingGroupArn"
                  Prelude..= autoScalingGroupArn
              )
          ]
      )
