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
-- Module      : Amazonka.ECS.Types.AutoScalingGroupProviderUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.AutoScalingGroupProviderUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types.ManagedScaling
import Amazonka.ECS.Types.ManagedTerminationProtection
import qualified Amazonka.Prelude as Prelude

-- | The details of the Auto Scaling group capacity provider to update.
--
-- /See:/ 'newAutoScalingGroupProviderUpdate' smart constructor.
data AutoScalingGroupProviderUpdate = AutoScalingGroupProviderUpdate'
  { -- | The managed scaling settings for the Auto Scaling group capacity
    -- provider.
    managedScaling :: Prelude.Maybe ManagedScaling,
    -- | The managed termination protection setting to use for the Auto Scaling
    -- group capacity provider. This determines whether the Auto Scaling group
    -- has managed termination protection.
    --
    -- When using managed termination protection, managed scaling must also be
    -- used otherwise managed termination protection doesn\'t work.
    --
    -- When managed termination protection is enabled, Amazon ECS prevents the
    -- Amazon EC2 instances in an Auto Scaling group that contain tasks from
    -- being terminated during a scale-in action. The Auto Scaling group and
    -- each instance in the Auto Scaling group must have instance protection
    -- from scale-in actions enabled. For more information, see
    -- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance Protection>
    -- in the /Auto Scaling User Guide/.
    --
    -- When managed termination protection is disabled, your Amazon EC2
    -- instances aren\'t protected from termination when the Auto Scaling group
    -- scales in.
    managedTerminationProtection :: Prelude.Maybe ManagedTerminationProtection
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoScalingGroupProviderUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'managedScaling', 'autoScalingGroupProviderUpdate_managedScaling' - The managed scaling settings for the Auto Scaling group capacity
-- provider.
--
-- 'managedTerminationProtection', 'autoScalingGroupProviderUpdate_managedTerminationProtection' - The managed termination protection setting to use for the Auto Scaling
-- group capacity provider. This determines whether the Auto Scaling group
-- has managed termination protection.
--
-- When using managed termination protection, managed scaling must also be
-- used otherwise managed termination protection doesn\'t work.
--
-- When managed termination protection is enabled, Amazon ECS prevents the
-- Amazon EC2 instances in an Auto Scaling group that contain tasks from
-- being terminated during a scale-in action. The Auto Scaling group and
-- each instance in the Auto Scaling group must have instance protection
-- from scale-in actions enabled. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance Protection>
-- in the /Auto Scaling User Guide/.
--
-- When managed termination protection is disabled, your Amazon EC2
-- instances aren\'t protected from termination when the Auto Scaling group
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

-- | The managed scaling settings for the Auto Scaling group capacity
-- provider.
autoScalingGroupProviderUpdate_managedScaling :: Lens.Lens' AutoScalingGroupProviderUpdate (Prelude.Maybe ManagedScaling)
autoScalingGroupProviderUpdate_managedScaling = Lens.lens (\AutoScalingGroupProviderUpdate' {managedScaling} -> managedScaling) (\s@AutoScalingGroupProviderUpdate' {} a -> s {managedScaling = a} :: AutoScalingGroupProviderUpdate)

-- | The managed termination protection setting to use for the Auto Scaling
-- group capacity provider. This determines whether the Auto Scaling group
-- has managed termination protection.
--
-- When using managed termination protection, managed scaling must also be
-- used otherwise managed termination protection doesn\'t work.
--
-- When managed termination protection is enabled, Amazon ECS prevents the
-- Amazon EC2 instances in an Auto Scaling group that contain tasks from
-- being terminated during a scale-in action. The Auto Scaling group and
-- each instance in the Auto Scaling group must have instance protection
-- from scale-in actions enabled. For more information, see
-- <https://docs.aws.amazon.com/autoscaling/ec2/userguide/as-instance-termination.html#instance-protection Instance Protection>
-- in the /Auto Scaling User Guide/.
--
-- When managed termination protection is disabled, your Amazon EC2
-- instances aren\'t protected from termination when the Auto Scaling group
-- scales in.
autoScalingGroupProviderUpdate_managedTerminationProtection :: Lens.Lens' AutoScalingGroupProviderUpdate (Prelude.Maybe ManagedTerminationProtection)
autoScalingGroupProviderUpdate_managedTerminationProtection = Lens.lens (\AutoScalingGroupProviderUpdate' {managedTerminationProtection} -> managedTerminationProtection) (\s@AutoScalingGroupProviderUpdate' {} a -> s {managedTerminationProtection = a} :: AutoScalingGroupProviderUpdate)

instance
  Prelude.Hashable
    AutoScalingGroupProviderUpdate
  where
  hashWithSalt
    _salt
    AutoScalingGroupProviderUpdate' {..} =
      _salt
        `Prelude.hashWithSalt` managedScaling
        `Prelude.hashWithSalt` managedTerminationProtection

instance
  Prelude.NFData
    AutoScalingGroupProviderUpdate
  where
  rnf AutoScalingGroupProviderUpdate' {..} =
    Prelude.rnf managedScaling `Prelude.seq`
      Prelude.rnf managedTerminationProtection

instance Data.ToJSON AutoScalingGroupProviderUpdate where
  toJSON AutoScalingGroupProviderUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("managedScaling" Data..=)
              Prelude.<$> managedScaling,
            ("managedTerminationProtection" Data..=)
              Prelude.<$> managedTerminationProtection
          ]
      )
