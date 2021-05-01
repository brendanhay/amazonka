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
-- Module      : Network.AWS.ECS.Types.AutoScalingGroupProviderUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.AutoScalingGroupProviderUpdate where

import Network.AWS.ECS.Types.ManagedScaling
import Network.AWS.ECS.Types.ManagedTerminationProtection
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The details of the Auto Scaling group capacity provider to update.
--
-- /See:/ 'newAutoScalingGroupProviderUpdate' smart constructor.
data AutoScalingGroupProviderUpdate = AutoScalingGroupProviderUpdate'
  { managedScaling :: Prelude.Maybe ManagedScaling,
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
    managedTerminationProtection :: Prelude.Maybe ManagedTerminationProtection
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AutoScalingGroupProviderUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'managedScaling', 'autoScalingGroupProviderUpdate_managedScaling' - Undocumented member.
--
-- 'managedTerminationProtection', 'autoScalingGroupProviderUpdate_managedTerminationProtection' - The managed termination protection setting to use for the Auto Scaling
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
newAutoScalingGroupProviderUpdate ::
  AutoScalingGroupProviderUpdate
newAutoScalingGroupProviderUpdate =
  AutoScalingGroupProviderUpdate'
    { managedScaling =
        Prelude.Nothing,
      managedTerminationProtection =
        Prelude.Nothing
    }

-- | Undocumented member.
autoScalingGroupProviderUpdate_managedScaling :: Lens.Lens' AutoScalingGroupProviderUpdate (Prelude.Maybe ManagedScaling)
autoScalingGroupProviderUpdate_managedScaling = Lens.lens (\AutoScalingGroupProviderUpdate' {managedScaling} -> managedScaling) (\s@AutoScalingGroupProviderUpdate' {} a -> s {managedScaling = a} :: AutoScalingGroupProviderUpdate)

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
autoScalingGroupProviderUpdate_managedTerminationProtection :: Lens.Lens' AutoScalingGroupProviderUpdate (Prelude.Maybe ManagedTerminationProtection)
autoScalingGroupProviderUpdate_managedTerminationProtection = Lens.lens (\AutoScalingGroupProviderUpdate' {managedTerminationProtection} -> managedTerminationProtection) (\s@AutoScalingGroupProviderUpdate' {} a -> s {managedTerminationProtection = a} :: AutoScalingGroupProviderUpdate)

instance
  Prelude.Hashable
    AutoScalingGroupProviderUpdate

instance
  Prelude.NFData
    AutoScalingGroupProviderUpdate

instance
  Prelude.ToJSON
    AutoScalingGroupProviderUpdate
  where
  toJSON AutoScalingGroupProviderUpdate' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("managedScaling" Prelude..=)
              Prelude.<$> managedScaling,
            ("managedTerminationProtection" Prelude..=)
              Prelude.<$> managedTerminationProtection
          ]
      )
