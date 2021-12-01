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
-- Module      : Amazonka.ECS.Types.ManagedScaling
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.ManagedScaling where

import qualified Amazonka.Core as Core
import Amazonka.ECS.Types.ManagedScalingStatus
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The managed scaling settings for the Auto Scaling group capacity
-- provider.
--
-- When managed scaling is enabled, Amazon ECS manages the scale-in and
-- scale-out actions of the Auto Scaling group. Amazon ECS manages a target
-- tracking scaling policy using an Amazon ECS-managed CloudWatch metric
-- with the specified @targetCapacity@ value as the target value for the
-- metric. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/asg-capacity-providers.html#asg-capacity-providers-managed-scaling Using Managed Scaling>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- If managed scaling is disabled, the user must manage the scaling of the
-- Auto Scaling group.
--
-- /See:/ 'newManagedScaling' smart constructor.
data ManagedScaling = ManagedScaling'
  { -- | Whether or not to enable managed scaling for the capacity provider.
    status :: Prelude.Maybe ManagedScalingStatus,
    -- | The maximum number of container instances that Amazon ECS will scale in
    -- or scale out at one time. If this parameter is omitted, the default
    -- value of @10000@ is used.
    maximumScalingStepSize :: Prelude.Maybe Prelude.Natural,
    -- | The target capacity value for the capacity provider. The specified value
    -- must be greater than @0@ and less than or equal to @100@. A value of
    -- @100@ will result in the Amazon EC2 instances in your Auto Scaling group
    -- being completely utilized.
    targetCapacity :: Prelude.Maybe Prelude.Natural,
    -- | The minimum number of container instances that Amazon ECS will scale in
    -- or scale out at one time. If this parameter is omitted, the default
    -- value of @1@ is used.
    minimumScalingStepSize :: Prelude.Maybe Prelude.Natural,
    -- | The period of time, in seconds, after a newly launched Amazon EC2
    -- instance can contribute to CloudWatch metrics for Auto Scaling group. If
    -- this parameter is omitted, the default value of @300@ seconds is used.
    instanceWarmupPeriod :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ManagedScaling' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'managedScaling_status' - Whether or not to enable managed scaling for the capacity provider.
--
-- 'maximumScalingStepSize', 'managedScaling_maximumScalingStepSize' - The maximum number of container instances that Amazon ECS will scale in
-- or scale out at one time. If this parameter is omitted, the default
-- value of @10000@ is used.
--
-- 'targetCapacity', 'managedScaling_targetCapacity' - The target capacity value for the capacity provider. The specified value
-- must be greater than @0@ and less than or equal to @100@. A value of
-- @100@ will result in the Amazon EC2 instances in your Auto Scaling group
-- being completely utilized.
--
-- 'minimumScalingStepSize', 'managedScaling_minimumScalingStepSize' - The minimum number of container instances that Amazon ECS will scale in
-- or scale out at one time. If this parameter is omitted, the default
-- value of @1@ is used.
--
-- 'instanceWarmupPeriod', 'managedScaling_instanceWarmupPeriod' - The period of time, in seconds, after a newly launched Amazon EC2
-- instance can contribute to CloudWatch metrics for Auto Scaling group. If
-- this parameter is omitted, the default value of @300@ seconds is used.
newManagedScaling ::
  ManagedScaling
newManagedScaling =
  ManagedScaling'
    { status = Prelude.Nothing,
      maximumScalingStepSize = Prelude.Nothing,
      targetCapacity = Prelude.Nothing,
      minimumScalingStepSize = Prelude.Nothing,
      instanceWarmupPeriod = Prelude.Nothing
    }

-- | Whether or not to enable managed scaling for the capacity provider.
managedScaling_status :: Lens.Lens' ManagedScaling (Prelude.Maybe ManagedScalingStatus)
managedScaling_status = Lens.lens (\ManagedScaling' {status} -> status) (\s@ManagedScaling' {} a -> s {status = a} :: ManagedScaling)

-- | The maximum number of container instances that Amazon ECS will scale in
-- or scale out at one time. If this parameter is omitted, the default
-- value of @10000@ is used.
managedScaling_maximumScalingStepSize :: Lens.Lens' ManagedScaling (Prelude.Maybe Prelude.Natural)
managedScaling_maximumScalingStepSize = Lens.lens (\ManagedScaling' {maximumScalingStepSize} -> maximumScalingStepSize) (\s@ManagedScaling' {} a -> s {maximumScalingStepSize = a} :: ManagedScaling)

-- | The target capacity value for the capacity provider. The specified value
-- must be greater than @0@ and less than or equal to @100@. A value of
-- @100@ will result in the Amazon EC2 instances in your Auto Scaling group
-- being completely utilized.
managedScaling_targetCapacity :: Lens.Lens' ManagedScaling (Prelude.Maybe Prelude.Natural)
managedScaling_targetCapacity = Lens.lens (\ManagedScaling' {targetCapacity} -> targetCapacity) (\s@ManagedScaling' {} a -> s {targetCapacity = a} :: ManagedScaling)

-- | The minimum number of container instances that Amazon ECS will scale in
-- or scale out at one time. If this parameter is omitted, the default
-- value of @1@ is used.
managedScaling_minimumScalingStepSize :: Lens.Lens' ManagedScaling (Prelude.Maybe Prelude.Natural)
managedScaling_minimumScalingStepSize = Lens.lens (\ManagedScaling' {minimumScalingStepSize} -> minimumScalingStepSize) (\s@ManagedScaling' {} a -> s {minimumScalingStepSize = a} :: ManagedScaling)

-- | The period of time, in seconds, after a newly launched Amazon EC2
-- instance can contribute to CloudWatch metrics for Auto Scaling group. If
-- this parameter is omitted, the default value of @300@ seconds is used.
managedScaling_instanceWarmupPeriod :: Lens.Lens' ManagedScaling (Prelude.Maybe Prelude.Natural)
managedScaling_instanceWarmupPeriod = Lens.lens (\ManagedScaling' {instanceWarmupPeriod} -> instanceWarmupPeriod) (\s@ManagedScaling' {} a -> s {instanceWarmupPeriod = a} :: ManagedScaling)

instance Core.FromJSON ManagedScaling where
  parseJSON =
    Core.withObject
      "ManagedScaling"
      ( \x ->
          ManagedScaling'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "maximumScalingStepSize")
            Prelude.<*> (x Core..:? "targetCapacity")
            Prelude.<*> (x Core..:? "minimumScalingStepSize")
            Prelude.<*> (x Core..:? "instanceWarmupPeriod")
      )

instance Prelude.Hashable ManagedScaling where
  hashWithSalt salt' ManagedScaling' {..} =
    salt' `Prelude.hashWithSalt` instanceWarmupPeriod
      `Prelude.hashWithSalt` minimumScalingStepSize
      `Prelude.hashWithSalt` targetCapacity
      `Prelude.hashWithSalt` maximumScalingStepSize
      `Prelude.hashWithSalt` status

instance Prelude.NFData ManagedScaling where
  rnf ManagedScaling' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf instanceWarmupPeriod
      `Prelude.seq` Prelude.rnf minimumScalingStepSize
      `Prelude.seq` Prelude.rnf targetCapacity
      `Prelude.seq` Prelude.rnf maximumScalingStepSize

instance Core.ToJSON ManagedScaling where
  toJSON ManagedScaling' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("status" Core..=) Prelude.<$> status,
            ("maximumScalingStepSize" Core..=)
              Prelude.<$> maximumScalingStepSize,
            ("targetCapacity" Core..=)
              Prelude.<$> targetCapacity,
            ("minimumScalingStepSize" Core..=)
              Prelude.<$> minimumScalingStepSize,
            ("instanceWarmupPeriod" Core..=)
              Prelude.<$> instanceWarmupPeriod
          ]
      )
