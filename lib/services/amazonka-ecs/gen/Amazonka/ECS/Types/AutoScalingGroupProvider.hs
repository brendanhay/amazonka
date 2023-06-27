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
-- Module      : Amazonka.ECS.Types.AutoScalingGroupProvider
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.AutoScalingGroupProvider where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types.ManagedScaling
import Amazonka.ECS.Types.ManagedTerminationProtection
import qualified Amazonka.Prelude as Prelude

-- | The details of the Auto Scaling group for the capacity provider.
--
-- /See:/ 'newAutoScalingGroupProvider' smart constructor.
data AutoScalingGroupProvider = AutoScalingGroupProvider'
  { -- | The managed scaling settings for the Auto Scaling group capacity
    -- provider.
    managedScaling :: Prelude.Maybe ManagedScaling,
    -- | The managed termination protection setting to use for the Auto Scaling
    -- group capacity provider. This determines whether the Auto Scaling group
    -- has managed termination protection. The default is off.
    --
    -- When using managed termination protection, managed scaling must also be
    -- used otherwise managed termination protection doesn\'t work.
    --
    -- When managed termination protection is on, Amazon ECS prevents the
    -- Amazon EC2 instances in an Auto Scaling group that contain tasks from
    -- being terminated during a scale-in action. The Auto Scaling group and
    -- each instance in the Auto Scaling group must have instance protection
    -- from scale-in actions on as well. For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance Protection>
    -- in the /Auto Scaling User Guide/.
    --
    -- When managed termination protection is off, your Amazon EC2 instances
    -- aren\'t protected from termination when the Auto Scaling group scales
    -- in.
    managedTerminationProtection :: Prelude.Maybe ManagedTerminationProtection,
    -- | The Amazon Resource Name (ARN) that identifies the Auto Scaling group.
    autoScalingGroupArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- has managed termination protection. The default is off.
--
-- When using managed termination protection, managed scaling must also be
-- used otherwise managed termination protection doesn\'t work.
--
-- When managed termination protection is on, Amazon ECS prevents the
-- Amazon EC2 instances in an Auto Scaling group that contain tasks from
-- being terminated during a scale-in action. The Auto Scaling group and
-- each instance in the Auto Scaling group must have instance protection
-- from scale-in actions on as well. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance Protection>
-- in the /Auto Scaling User Guide/.
--
-- When managed termination protection is off, your Amazon EC2 instances
-- aren\'t protected from termination when the Auto Scaling group scales
-- in.
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
-- has managed termination protection. The default is off.
--
-- When using managed termination protection, managed scaling must also be
-- used otherwise managed termination protection doesn\'t work.
--
-- When managed termination protection is on, Amazon ECS prevents the
-- Amazon EC2 instances in an Auto Scaling group that contain tasks from
-- being terminated during a scale-in action. The Auto Scaling group and
-- each instance in the Auto Scaling group must have instance protection
-- from scale-in actions on as well. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance Protection>
-- in the /Auto Scaling User Guide/.
--
-- When managed termination protection is off, your Amazon EC2 instances
-- aren\'t protected from termination when the Auto Scaling group scales
-- in.
autoScalingGroupProvider_managedTerminationProtection :: Lens.Lens' AutoScalingGroupProvider (Prelude.Maybe ManagedTerminationProtection)
autoScalingGroupProvider_managedTerminationProtection = Lens.lens (\AutoScalingGroupProvider' {managedTerminationProtection} -> managedTerminationProtection) (\s@AutoScalingGroupProvider' {} a -> s {managedTerminationProtection = a} :: AutoScalingGroupProvider)

-- | The Amazon Resource Name (ARN) that identifies the Auto Scaling group.
autoScalingGroupProvider_autoScalingGroupArn :: Lens.Lens' AutoScalingGroupProvider Prelude.Text
autoScalingGroupProvider_autoScalingGroupArn = Lens.lens (\AutoScalingGroupProvider' {autoScalingGroupArn} -> autoScalingGroupArn) (\s@AutoScalingGroupProvider' {} a -> s {autoScalingGroupArn = a} :: AutoScalingGroupProvider)

instance Data.FromJSON AutoScalingGroupProvider where
  parseJSON =
    Data.withObject
      "AutoScalingGroupProvider"
      ( \x ->
          AutoScalingGroupProvider'
            Prelude.<$> (x Data..:? "managedScaling")
            Prelude.<*> (x Data..:? "managedTerminationProtection")
            Prelude.<*> (x Data..: "autoScalingGroupArn")
      )

instance Prelude.Hashable AutoScalingGroupProvider where
  hashWithSalt _salt AutoScalingGroupProvider' {..} =
    _salt
      `Prelude.hashWithSalt` managedScaling
      `Prelude.hashWithSalt` managedTerminationProtection
      `Prelude.hashWithSalt` autoScalingGroupArn

instance Prelude.NFData AutoScalingGroupProvider where
  rnf AutoScalingGroupProvider' {..} =
    Prelude.rnf managedScaling
      `Prelude.seq` Prelude.rnf managedTerminationProtection
      `Prelude.seq` Prelude.rnf autoScalingGroupArn

instance Data.ToJSON AutoScalingGroupProvider where
  toJSON AutoScalingGroupProvider' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("managedScaling" Data..=)
              Prelude.<$> managedScaling,
            ("managedTerminationProtection" Data..=)
              Prelude.<$> managedTerminationProtection,
            Prelude.Just
              ("autoScalingGroupArn" Data..= autoScalingGroupArn)
          ]
      )
