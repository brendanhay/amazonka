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
-- Module      : Amazonka.AutoScaling.Types.InstancesDistribution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.InstancesDistribution where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes an instances distribution for an Auto Scaling group with a
-- MixedInstancesPolicy.
--
-- The instances distribution specifies the distribution of On-Demand
-- Instances and Spot Instances, the maximum price to pay for Spot
-- Instances, and how the Auto Scaling group allocates instance types to
-- fulfill On-Demand and Spot capacities.
--
-- When you modify @SpotAllocationStrategy@, @SpotInstancePools@, or
-- @SpotMaxPrice@ in the UpdateAutoScalingGroup API call, this update
-- action does not deploy any changes across the running Amazon EC2
-- instances in the group. Your existing Spot Instances continue to run as
-- long as the maximum price for those instances is higher than the current
-- Spot price. When scale out occurs, Amazon EC2 Auto Scaling launches
-- instances based on the new settings. When scale in occurs, Amazon EC2
-- Auto Scaling terminates instances according to the group\'s termination
-- policies.
--
-- /See:/ 'newInstancesDistribution' smart constructor.
data InstancesDistribution = InstancesDistribution'
  { -- | Indicates how to allocate instances across Spot Instance pools.
    --
    -- If the allocation strategy is @lowest-price@, the Auto Scaling group
    -- launches instances using the Spot pools with the lowest price, and
    -- evenly allocates your instances across the number of Spot pools that you
    -- specify. Defaults to @lowest-price@ if not specified.
    --
    -- If the allocation strategy is @capacity-optimized@ (recommended), the
    -- Auto Scaling group launches instances using Spot pools that are
    -- optimally chosen based on the available Spot capacity. Alternatively,
    -- you can use @capacity-optimized-prioritized@ and set the order of
    -- instance types in the list of launch template overrides from highest to
    -- lowest priority (from first to last in the list). Amazon EC2 Auto
    -- Scaling honors the instance type priorities on a best-effort basis but
    -- optimizes for capacity first.
    spotAllocationStrategy :: Prelude.Maybe Prelude.Text,
    -- | The number of Spot Instance pools across which to allocate your Spot
    -- Instances. The Spot pools are determined from the different instance
    -- types in the overrides. Valid only when the Spot allocation strategy is
    -- @lowest-price@. Value must be in the range of 1 to 20. Defaults to 2 if
    -- not specified.
    spotInstancePools :: Prelude.Maybe Prelude.Int,
    -- | The maximum price per unit hour that you are willing to pay for a Spot
    -- Instance. If you leave the value at its default (empty), Amazon EC2 Auto
    -- Scaling uses the On-Demand price as the maximum Spot price. To remove a
    -- value that you previously set, include the property but specify an empty
    -- string (\"\") for the value.
    spotMaxPrice :: Prelude.Maybe Prelude.Text,
    -- | The minimum amount of the Auto Scaling group\'s capacity that must be
    -- fulfilled by On-Demand Instances. This base portion is provisioned first
    -- as your group scales. Defaults to 0 if not specified. If you specify
    -- weights for the instance types in the overrides, set the value of
    -- @OnDemandBaseCapacity@ in terms of the number of capacity units, and not
    -- the number of instances.
    onDemandBaseCapacity :: Prelude.Maybe Prelude.Int,
    -- | Indicates how to allocate instance types to fulfill On-Demand capacity.
    -- The only valid value is @prioritized@, which is also the default value.
    -- This strategy uses the order of instance types in the
    -- @LaunchTemplateOverrides@ to define the launch priority of each instance
    -- type. The first instance type in the array is prioritized higher than
    -- the last. If all your On-Demand capacity cannot be fulfilled using your
    -- highest priority instance, then the Auto Scaling groups launches the
    -- remaining capacity using the second priority instance type, and so on.
    onDemandAllocationStrategy :: Prelude.Maybe Prelude.Text,
    -- | Controls the percentages of On-Demand Instances and Spot Instances for
    -- your additional capacity beyond @OnDemandBaseCapacity@. Expressed as a
    -- number (for example, 20 specifies 20% On-Demand Instances, 80% Spot
    -- Instances). Defaults to 100 if not specified. If set to 100, only
    -- On-Demand Instances are provisioned.
    onDemandPercentageAboveBaseCapacity :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstancesDistribution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'spotAllocationStrategy', 'instancesDistribution_spotAllocationStrategy' - Indicates how to allocate instances across Spot Instance pools.
--
-- If the allocation strategy is @lowest-price@, the Auto Scaling group
-- launches instances using the Spot pools with the lowest price, and
-- evenly allocates your instances across the number of Spot pools that you
-- specify. Defaults to @lowest-price@ if not specified.
--
-- If the allocation strategy is @capacity-optimized@ (recommended), the
-- Auto Scaling group launches instances using Spot pools that are
-- optimally chosen based on the available Spot capacity. Alternatively,
-- you can use @capacity-optimized-prioritized@ and set the order of
-- instance types in the list of launch template overrides from highest to
-- lowest priority (from first to last in the list). Amazon EC2 Auto
-- Scaling honors the instance type priorities on a best-effort basis but
-- optimizes for capacity first.
--
-- 'spotInstancePools', 'instancesDistribution_spotInstancePools' - The number of Spot Instance pools across which to allocate your Spot
-- Instances. The Spot pools are determined from the different instance
-- types in the overrides. Valid only when the Spot allocation strategy is
-- @lowest-price@. Value must be in the range of 1 to 20. Defaults to 2 if
-- not specified.
--
-- 'spotMaxPrice', 'instancesDistribution_spotMaxPrice' - The maximum price per unit hour that you are willing to pay for a Spot
-- Instance. If you leave the value at its default (empty), Amazon EC2 Auto
-- Scaling uses the On-Demand price as the maximum Spot price. To remove a
-- value that you previously set, include the property but specify an empty
-- string (\"\") for the value.
--
-- 'onDemandBaseCapacity', 'instancesDistribution_onDemandBaseCapacity' - The minimum amount of the Auto Scaling group\'s capacity that must be
-- fulfilled by On-Demand Instances. This base portion is provisioned first
-- as your group scales. Defaults to 0 if not specified. If you specify
-- weights for the instance types in the overrides, set the value of
-- @OnDemandBaseCapacity@ in terms of the number of capacity units, and not
-- the number of instances.
--
-- 'onDemandAllocationStrategy', 'instancesDistribution_onDemandAllocationStrategy' - Indicates how to allocate instance types to fulfill On-Demand capacity.
-- The only valid value is @prioritized@, which is also the default value.
-- This strategy uses the order of instance types in the
-- @LaunchTemplateOverrides@ to define the launch priority of each instance
-- type. The first instance type in the array is prioritized higher than
-- the last. If all your On-Demand capacity cannot be fulfilled using your
-- highest priority instance, then the Auto Scaling groups launches the
-- remaining capacity using the second priority instance type, and so on.
--
-- 'onDemandPercentageAboveBaseCapacity', 'instancesDistribution_onDemandPercentageAboveBaseCapacity' - Controls the percentages of On-Demand Instances and Spot Instances for
-- your additional capacity beyond @OnDemandBaseCapacity@. Expressed as a
-- number (for example, 20 specifies 20% On-Demand Instances, 80% Spot
-- Instances). Defaults to 100 if not specified. If set to 100, only
-- On-Demand Instances are provisioned.
newInstancesDistribution ::
  InstancesDistribution
newInstancesDistribution =
  InstancesDistribution'
    { spotAllocationStrategy =
        Prelude.Nothing,
      spotInstancePools = Prelude.Nothing,
      spotMaxPrice = Prelude.Nothing,
      onDemandBaseCapacity = Prelude.Nothing,
      onDemandAllocationStrategy = Prelude.Nothing,
      onDemandPercentageAboveBaseCapacity =
        Prelude.Nothing
    }

-- | Indicates how to allocate instances across Spot Instance pools.
--
-- If the allocation strategy is @lowest-price@, the Auto Scaling group
-- launches instances using the Spot pools with the lowest price, and
-- evenly allocates your instances across the number of Spot pools that you
-- specify. Defaults to @lowest-price@ if not specified.
--
-- If the allocation strategy is @capacity-optimized@ (recommended), the
-- Auto Scaling group launches instances using Spot pools that are
-- optimally chosen based on the available Spot capacity. Alternatively,
-- you can use @capacity-optimized-prioritized@ and set the order of
-- instance types in the list of launch template overrides from highest to
-- lowest priority (from first to last in the list). Amazon EC2 Auto
-- Scaling honors the instance type priorities on a best-effort basis but
-- optimizes for capacity first.
instancesDistribution_spotAllocationStrategy :: Lens.Lens' InstancesDistribution (Prelude.Maybe Prelude.Text)
instancesDistribution_spotAllocationStrategy = Lens.lens (\InstancesDistribution' {spotAllocationStrategy} -> spotAllocationStrategy) (\s@InstancesDistribution' {} a -> s {spotAllocationStrategy = a} :: InstancesDistribution)

-- | The number of Spot Instance pools across which to allocate your Spot
-- Instances. The Spot pools are determined from the different instance
-- types in the overrides. Valid only when the Spot allocation strategy is
-- @lowest-price@. Value must be in the range of 1 to 20. Defaults to 2 if
-- not specified.
instancesDistribution_spotInstancePools :: Lens.Lens' InstancesDistribution (Prelude.Maybe Prelude.Int)
instancesDistribution_spotInstancePools = Lens.lens (\InstancesDistribution' {spotInstancePools} -> spotInstancePools) (\s@InstancesDistribution' {} a -> s {spotInstancePools = a} :: InstancesDistribution)

-- | The maximum price per unit hour that you are willing to pay for a Spot
-- Instance. If you leave the value at its default (empty), Amazon EC2 Auto
-- Scaling uses the On-Demand price as the maximum Spot price. To remove a
-- value that you previously set, include the property but specify an empty
-- string (\"\") for the value.
instancesDistribution_spotMaxPrice :: Lens.Lens' InstancesDistribution (Prelude.Maybe Prelude.Text)
instancesDistribution_spotMaxPrice = Lens.lens (\InstancesDistribution' {spotMaxPrice} -> spotMaxPrice) (\s@InstancesDistribution' {} a -> s {spotMaxPrice = a} :: InstancesDistribution)

-- | The minimum amount of the Auto Scaling group\'s capacity that must be
-- fulfilled by On-Demand Instances. This base portion is provisioned first
-- as your group scales. Defaults to 0 if not specified. If you specify
-- weights for the instance types in the overrides, set the value of
-- @OnDemandBaseCapacity@ in terms of the number of capacity units, and not
-- the number of instances.
instancesDistribution_onDemandBaseCapacity :: Lens.Lens' InstancesDistribution (Prelude.Maybe Prelude.Int)
instancesDistribution_onDemandBaseCapacity = Lens.lens (\InstancesDistribution' {onDemandBaseCapacity} -> onDemandBaseCapacity) (\s@InstancesDistribution' {} a -> s {onDemandBaseCapacity = a} :: InstancesDistribution)

-- | Indicates how to allocate instance types to fulfill On-Demand capacity.
-- The only valid value is @prioritized@, which is also the default value.
-- This strategy uses the order of instance types in the
-- @LaunchTemplateOverrides@ to define the launch priority of each instance
-- type. The first instance type in the array is prioritized higher than
-- the last. If all your On-Demand capacity cannot be fulfilled using your
-- highest priority instance, then the Auto Scaling groups launches the
-- remaining capacity using the second priority instance type, and so on.
instancesDistribution_onDemandAllocationStrategy :: Lens.Lens' InstancesDistribution (Prelude.Maybe Prelude.Text)
instancesDistribution_onDemandAllocationStrategy = Lens.lens (\InstancesDistribution' {onDemandAllocationStrategy} -> onDemandAllocationStrategy) (\s@InstancesDistribution' {} a -> s {onDemandAllocationStrategy = a} :: InstancesDistribution)

-- | Controls the percentages of On-Demand Instances and Spot Instances for
-- your additional capacity beyond @OnDemandBaseCapacity@. Expressed as a
-- number (for example, 20 specifies 20% On-Demand Instances, 80% Spot
-- Instances). Defaults to 100 if not specified. If set to 100, only
-- On-Demand Instances are provisioned.
instancesDistribution_onDemandPercentageAboveBaseCapacity :: Lens.Lens' InstancesDistribution (Prelude.Maybe Prelude.Int)
instancesDistribution_onDemandPercentageAboveBaseCapacity = Lens.lens (\InstancesDistribution' {onDemandPercentageAboveBaseCapacity} -> onDemandPercentageAboveBaseCapacity) (\s@InstancesDistribution' {} a -> s {onDemandPercentageAboveBaseCapacity = a} :: InstancesDistribution)

instance Core.FromXML InstancesDistribution where
  parseXML x =
    InstancesDistribution'
      Prelude.<$> (x Core..@? "SpotAllocationStrategy")
      Prelude.<*> (x Core..@? "SpotInstancePools")
      Prelude.<*> (x Core..@? "SpotMaxPrice")
      Prelude.<*> (x Core..@? "OnDemandBaseCapacity")
      Prelude.<*> (x Core..@? "OnDemandAllocationStrategy")
      Prelude.<*> (x Core..@? "OnDemandPercentageAboveBaseCapacity")

instance Prelude.Hashable InstancesDistribution where
  hashWithSalt _salt InstancesDistribution' {..} =
    _salt `Prelude.hashWithSalt` spotAllocationStrategy
      `Prelude.hashWithSalt` spotInstancePools
      `Prelude.hashWithSalt` spotMaxPrice
      `Prelude.hashWithSalt` onDemandBaseCapacity
      `Prelude.hashWithSalt` onDemandAllocationStrategy
      `Prelude.hashWithSalt` onDemandPercentageAboveBaseCapacity

instance Prelude.NFData InstancesDistribution where
  rnf InstancesDistribution' {..} =
    Prelude.rnf spotAllocationStrategy
      `Prelude.seq` Prelude.rnf spotInstancePools
      `Prelude.seq` Prelude.rnf spotMaxPrice
      `Prelude.seq` Prelude.rnf onDemandBaseCapacity
      `Prelude.seq` Prelude.rnf onDemandAllocationStrategy
      `Prelude.seq` Prelude.rnf onDemandPercentageAboveBaseCapacity

instance Core.ToQuery InstancesDistribution where
  toQuery InstancesDistribution' {..} =
    Prelude.mconcat
      [ "SpotAllocationStrategy"
          Core.=: spotAllocationStrategy,
        "SpotInstancePools" Core.=: spotInstancePools,
        "SpotMaxPrice" Core.=: spotMaxPrice,
        "OnDemandBaseCapacity" Core.=: onDemandBaseCapacity,
        "OnDemandAllocationStrategy"
          Core.=: onDemandAllocationStrategy,
        "OnDemandPercentageAboveBaseCapacity"
          Core.=: onDemandPercentageAboveBaseCapacity
      ]
