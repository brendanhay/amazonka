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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.ManagedScaling where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types.ManagedScalingStatus
import qualified Amazonka.Prelude as Prelude

-- | The managed scaling settings for the Auto Scaling group capacity
-- provider.
--
-- When managed scaling is enabled, Amazon ECS manages the scale-in and
-- scale-out actions of the Auto Scaling group. Amazon ECS manages a target
-- tracking scaling policy using an Amazon ECS managed CloudWatch metric
-- with the specified @targetCapacity@ value as the target value for the
-- metric. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/asg-capacity-providers.html#asg-capacity-providers-managed-scaling Using managed scaling>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- If managed scaling is disabled, the user must manage the scaling of the
-- Auto Scaling group.
--
-- /See:/ 'newManagedScaling' smart constructor.
data ManagedScaling = ManagedScaling'
  { -- | The target capacity value for the capacity provider. The specified value
    -- must be greater than @0@ and less than or equal to @100@. A value of
    -- @100@ results in the Amazon EC2 instances in your Auto Scaling group
    -- being completely used.
    targetCapacity :: Prelude.Maybe Prelude.Natural,
    -- | The period of time, in seconds, after a newly launched Amazon EC2
    -- instance can contribute to CloudWatch metrics for Auto Scaling group. If
    -- this parameter is omitted, the default value of @300@ seconds is used.
    instanceWarmupPeriod :: Prelude.Maybe Prelude.Natural,
    -- | Determines whether to use managed scaling for the capacity provider.
    status :: Prelude.Maybe ManagedScalingStatus,
    -- | The minimum number of Amazon EC2 instances that Amazon ECS will scale
    -- out at one time. The scale in process is not affected by this parameter
    -- If this parameter is omitted, the default value of @1@ is used.
    --
    -- When additional capacity is required, Amazon ECS will scale up the
    -- minimum scaling step size even if the actual demand is less than the
    -- minimum scaling step size.
    --
    -- If you use a capacity provider with an Auto Scaling group configured
    -- with more than one Amazon EC2 instance type or Availability Zone, Amazon
    -- ECS will scale up by the exact minimum scaling step size value and will
    -- ignore both the maximum scaling step size as well as the capacity
    -- demand.
    minimumScalingStepSize :: Prelude.Maybe Prelude.Natural,
    -- | The maximum number of Amazon EC2 instances that Amazon ECS will scale
    -- out at one time. The scale in process is not affected by this parameter.
    -- If this parameter is omitted, the default value of @10000@ is used.
    maximumScalingStepSize :: Prelude.Maybe Prelude.Natural
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
-- 'targetCapacity', 'managedScaling_targetCapacity' - The target capacity value for the capacity provider. The specified value
-- must be greater than @0@ and less than or equal to @100@. A value of
-- @100@ results in the Amazon EC2 instances in your Auto Scaling group
-- being completely used.
--
-- 'instanceWarmupPeriod', 'managedScaling_instanceWarmupPeriod' - The period of time, in seconds, after a newly launched Amazon EC2
-- instance can contribute to CloudWatch metrics for Auto Scaling group. If
-- this parameter is omitted, the default value of @300@ seconds is used.
--
-- 'status', 'managedScaling_status' - Determines whether to use managed scaling for the capacity provider.
--
-- 'minimumScalingStepSize', 'managedScaling_minimumScalingStepSize' - The minimum number of Amazon EC2 instances that Amazon ECS will scale
-- out at one time. The scale in process is not affected by this parameter
-- If this parameter is omitted, the default value of @1@ is used.
--
-- When additional capacity is required, Amazon ECS will scale up the
-- minimum scaling step size even if the actual demand is less than the
-- minimum scaling step size.
--
-- If you use a capacity provider with an Auto Scaling group configured
-- with more than one Amazon EC2 instance type or Availability Zone, Amazon
-- ECS will scale up by the exact minimum scaling step size value and will
-- ignore both the maximum scaling step size as well as the capacity
-- demand.
--
-- 'maximumScalingStepSize', 'managedScaling_maximumScalingStepSize' - The maximum number of Amazon EC2 instances that Amazon ECS will scale
-- out at one time. The scale in process is not affected by this parameter.
-- If this parameter is omitted, the default value of @10000@ is used.
newManagedScaling ::
  ManagedScaling
newManagedScaling =
  ManagedScaling'
    { targetCapacity = Prelude.Nothing,
      instanceWarmupPeriod = Prelude.Nothing,
      status = Prelude.Nothing,
      minimumScalingStepSize = Prelude.Nothing,
      maximumScalingStepSize = Prelude.Nothing
    }

-- | The target capacity value for the capacity provider. The specified value
-- must be greater than @0@ and less than or equal to @100@. A value of
-- @100@ results in the Amazon EC2 instances in your Auto Scaling group
-- being completely used.
managedScaling_targetCapacity :: Lens.Lens' ManagedScaling (Prelude.Maybe Prelude.Natural)
managedScaling_targetCapacity = Lens.lens (\ManagedScaling' {targetCapacity} -> targetCapacity) (\s@ManagedScaling' {} a -> s {targetCapacity = a} :: ManagedScaling)

-- | The period of time, in seconds, after a newly launched Amazon EC2
-- instance can contribute to CloudWatch metrics for Auto Scaling group. If
-- this parameter is omitted, the default value of @300@ seconds is used.
managedScaling_instanceWarmupPeriod :: Lens.Lens' ManagedScaling (Prelude.Maybe Prelude.Natural)
managedScaling_instanceWarmupPeriod = Lens.lens (\ManagedScaling' {instanceWarmupPeriod} -> instanceWarmupPeriod) (\s@ManagedScaling' {} a -> s {instanceWarmupPeriod = a} :: ManagedScaling)

-- | Determines whether to use managed scaling for the capacity provider.
managedScaling_status :: Lens.Lens' ManagedScaling (Prelude.Maybe ManagedScalingStatus)
managedScaling_status = Lens.lens (\ManagedScaling' {status} -> status) (\s@ManagedScaling' {} a -> s {status = a} :: ManagedScaling)

-- | The minimum number of Amazon EC2 instances that Amazon ECS will scale
-- out at one time. The scale in process is not affected by this parameter
-- If this parameter is omitted, the default value of @1@ is used.
--
-- When additional capacity is required, Amazon ECS will scale up the
-- minimum scaling step size even if the actual demand is less than the
-- minimum scaling step size.
--
-- If you use a capacity provider with an Auto Scaling group configured
-- with more than one Amazon EC2 instance type or Availability Zone, Amazon
-- ECS will scale up by the exact minimum scaling step size value and will
-- ignore both the maximum scaling step size as well as the capacity
-- demand.
managedScaling_minimumScalingStepSize :: Lens.Lens' ManagedScaling (Prelude.Maybe Prelude.Natural)
managedScaling_minimumScalingStepSize = Lens.lens (\ManagedScaling' {minimumScalingStepSize} -> minimumScalingStepSize) (\s@ManagedScaling' {} a -> s {minimumScalingStepSize = a} :: ManagedScaling)

-- | The maximum number of Amazon EC2 instances that Amazon ECS will scale
-- out at one time. The scale in process is not affected by this parameter.
-- If this parameter is omitted, the default value of @10000@ is used.
managedScaling_maximumScalingStepSize :: Lens.Lens' ManagedScaling (Prelude.Maybe Prelude.Natural)
managedScaling_maximumScalingStepSize = Lens.lens (\ManagedScaling' {maximumScalingStepSize} -> maximumScalingStepSize) (\s@ManagedScaling' {} a -> s {maximumScalingStepSize = a} :: ManagedScaling)

instance Data.FromJSON ManagedScaling where
  parseJSON =
    Data.withObject
      "ManagedScaling"
      ( \x ->
          ManagedScaling'
            Prelude.<$> (x Data..:? "targetCapacity")
            Prelude.<*> (x Data..:? "instanceWarmupPeriod")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "minimumScalingStepSize")
            Prelude.<*> (x Data..:? "maximumScalingStepSize")
      )

instance Prelude.Hashable ManagedScaling where
  hashWithSalt _salt ManagedScaling' {..} =
    _salt `Prelude.hashWithSalt` targetCapacity
      `Prelude.hashWithSalt` instanceWarmupPeriod
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` minimumScalingStepSize
      `Prelude.hashWithSalt` maximumScalingStepSize

instance Prelude.NFData ManagedScaling where
  rnf ManagedScaling' {..} =
    Prelude.rnf targetCapacity
      `Prelude.seq` Prelude.rnf instanceWarmupPeriod
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf minimumScalingStepSize
      `Prelude.seq` Prelude.rnf maximumScalingStepSize

instance Data.ToJSON ManagedScaling where
  toJSON ManagedScaling' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("targetCapacity" Data..=)
              Prelude.<$> targetCapacity,
            ("instanceWarmupPeriod" Data..=)
              Prelude.<$> instanceWarmupPeriod,
            ("status" Data..=) Prelude.<$> status,
            ("minimumScalingStepSize" Data..=)
              Prelude.<$> minimumScalingStepSize,
            ("maximumScalingStepSize" Data..=)
              Prelude.<$> maximumScalingStepSize
          ]
      )
