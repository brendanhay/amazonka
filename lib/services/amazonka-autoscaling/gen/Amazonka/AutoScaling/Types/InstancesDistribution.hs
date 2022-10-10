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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.InstancesDistribution where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes an instances distribution for an Auto Scaling group.
--
-- /See:/ 'newInstancesDistribution' smart constructor.
data InstancesDistribution = InstancesDistribution'
  { -- | The minimum amount of the Auto Scaling group\'s capacity that must be
    -- fulfilled by On-Demand Instances. This base portion is launched first as
    -- your group scales.
    --
    -- If you specify weights for the instance types in the overrides, the base
    -- capacity is measured in the same unit of measurement as the instance
    -- types. If you specify InstanceRequirements in the overrides, the base
    -- capacity is measured in the same unit of measurement as your group\'s
    -- desired capacity.
    --
    -- Default: @0@
    onDemandBaseCapacity :: Prelude.Maybe Prelude.Int,
    -- | Controls the percentages of On-Demand Instances and Spot Instances for
    -- your additional capacity beyond @OnDemandBaseCapacity@. Expressed as a
    -- number (for example, 20 specifies 20% On-Demand Instances, 80% Spot
    -- Instances). If set to 100, only On-Demand Instances are used.
    --
    -- Default: @100@
    onDemandPercentageAboveBaseCapacity :: Prelude.Maybe Prelude.Int,
    -- | The order of the launch template overrides to use in fulfilling
    -- On-Demand capacity.
    --
    -- If you specify @lowest-price@, Amazon EC2 Auto Scaling uses price to
    -- determine the order, launching the lowest price first.
    --
    -- If you specify @prioritized@, Amazon EC2 Auto Scaling uses the priority
    -- that you assigned to each launch template override, launching the
    -- highest priority first. If all your On-Demand capacity cannot be
    -- fulfilled using your highest priority instance, then Amazon EC2 Auto
    -- Scaling launches the remaining capacity using the second priority
    -- instance type, and so on.
    --
    -- Default: @lowest-price@ for Auto Scaling groups that specify
    -- InstanceRequirements in the overrides and @prioritized@ for Auto Scaling
    -- groups that don\'t.
    --
    -- Valid values: @lowest-price@ | @prioritized@
    onDemandAllocationStrategy :: Prelude.Maybe Prelude.Text,
    -- | The number of Spot Instance pools across which to allocate your Spot
    -- Instances. The Spot pools are determined from the different instance
    -- types in the overrides. Valid only when the Spot allocation strategy is
    -- @lowest-price@. Value must be in the range of 1–20.
    --
    -- Default: @2@
    spotInstancePools :: Prelude.Maybe Prelude.Int,
    -- | The maximum price per unit hour that you are willing to pay for a Spot
    -- Instance. If you keep the value at its default (unspecified), Amazon EC2
    -- Auto Scaling uses the On-Demand price as the maximum Spot price. To
    -- remove a value that you previously set, include the property but specify
    -- an empty string (\"\") for the value.
    --
    -- If your maximum price is lower than the Spot price for the instance
    -- types that you selected, your Spot Instances are not launched.
    --
    -- Valid Range: Minimum value of 0.001
    spotMaxPrice :: Prelude.Maybe Prelude.Text,
    -- | Indicates how to allocate instances across Spot Instance pools.
    --
    -- If the allocation strategy is @lowest-price@, the Auto Scaling group
    -- launches instances using the Spot pools with the lowest price, and
    -- evenly allocates your instances across the number of Spot pools that you
    -- specify.
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
    -- Default: @lowest-price@
    --
    -- Valid values: @lowest-price@ | @capacity-optimized@ |
    -- @capacity-optimized-prioritized@
    spotAllocationStrategy :: Prelude.Maybe Prelude.Text
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
-- 'onDemandBaseCapacity', 'instancesDistribution_onDemandBaseCapacity' - The minimum amount of the Auto Scaling group\'s capacity that must be
-- fulfilled by On-Demand Instances. This base portion is launched first as
-- your group scales.
--
-- If you specify weights for the instance types in the overrides, the base
-- capacity is measured in the same unit of measurement as the instance
-- types. If you specify InstanceRequirements in the overrides, the base
-- capacity is measured in the same unit of measurement as your group\'s
-- desired capacity.
--
-- Default: @0@
--
-- 'onDemandPercentageAboveBaseCapacity', 'instancesDistribution_onDemandPercentageAboveBaseCapacity' - Controls the percentages of On-Demand Instances and Spot Instances for
-- your additional capacity beyond @OnDemandBaseCapacity@. Expressed as a
-- number (for example, 20 specifies 20% On-Demand Instances, 80% Spot
-- Instances). If set to 100, only On-Demand Instances are used.
--
-- Default: @100@
--
-- 'onDemandAllocationStrategy', 'instancesDistribution_onDemandAllocationStrategy' - The order of the launch template overrides to use in fulfilling
-- On-Demand capacity.
--
-- If you specify @lowest-price@, Amazon EC2 Auto Scaling uses price to
-- determine the order, launching the lowest price first.
--
-- If you specify @prioritized@, Amazon EC2 Auto Scaling uses the priority
-- that you assigned to each launch template override, launching the
-- highest priority first. If all your On-Demand capacity cannot be
-- fulfilled using your highest priority instance, then Amazon EC2 Auto
-- Scaling launches the remaining capacity using the second priority
-- instance type, and so on.
--
-- Default: @lowest-price@ for Auto Scaling groups that specify
-- InstanceRequirements in the overrides and @prioritized@ for Auto Scaling
-- groups that don\'t.
--
-- Valid values: @lowest-price@ | @prioritized@
--
-- 'spotInstancePools', 'instancesDistribution_spotInstancePools' - The number of Spot Instance pools across which to allocate your Spot
-- Instances. The Spot pools are determined from the different instance
-- types in the overrides. Valid only when the Spot allocation strategy is
-- @lowest-price@. Value must be in the range of 1–20.
--
-- Default: @2@
--
-- 'spotMaxPrice', 'instancesDistribution_spotMaxPrice' - The maximum price per unit hour that you are willing to pay for a Spot
-- Instance. If you keep the value at its default (unspecified), Amazon EC2
-- Auto Scaling uses the On-Demand price as the maximum Spot price. To
-- remove a value that you previously set, include the property but specify
-- an empty string (\"\") for the value.
--
-- If your maximum price is lower than the Spot price for the instance
-- types that you selected, your Spot Instances are not launched.
--
-- Valid Range: Minimum value of 0.001
--
-- 'spotAllocationStrategy', 'instancesDistribution_spotAllocationStrategy' - Indicates how to allocate instances across Spot Instance pools.
--
-- If the allocation strategy is @lowest-price@, the Auto Scaling group
-- launches instances using the Spot pools with the lowest price, and
-- evenly allocates your instances across the number of Spot pools that you
-- specify.
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
-- Default: @lowest-price@
--
-- Valid values: @lowest-price@ | @capacity-optimized@ |
-- @capacity-optimized-prioritized@
newInstancesDistribution ::
  InstancesDistribution
newInstancesDistribution =
  InstancesDistribution'
    { onDemandBaseCapacity =
        Prelude.Nothing,
      onDemandPercentageAboveBaseCapacity =
        Prelude.Nothing,
      onDemandAllocationStrategy = Prelude.Nothing,
      spotInstancePools = Prelude.Nothing,
      spotMaxPrice = Prelude.Nothing,
      spotAllocationStrategy = Prelude.Nothing
    }

-- | The minimum amount of the Auto Scaling group\'s capacity that must be
-- fulfilled by On-Demand Instances. This base portion is launched first as
-- your group scales.
--
-- If you specify weights for the instance types in the overrides, the base
-- capacity is measured in the same unit of measurement as the instance
-- types. If you specify InstanceRequirements in the overrides, the base
-- capacity is measured in the same unit of measurement as your group\'s
-- desired capacity.
--
-- Default: @0@
instancesDistribution_onDemandBaseCapacity :: Lens.Lens' InstancesDistribution (Prelude.Maybe Prelude.Int)
instancesDistribution_onDemandBaseCapacity = Lens.lens (\InstancesDistribution' {onDemandBaseCapacity} -> onDemandBaseCapacity) (\s@InstancesDistribution' {} a -> s {onDemandBaseCapacity = a} :: InstancesDistribution)

-- | Controls the percentages of On-Demand Instances and Spot Instances for
-- your additional capacity beyond @OnDemandBaseCapacity@. Expressed as a
-- number (for example, 20 specifies 20% On-Demand Instances, 80% Spot
-- Instances). If set to 100, only On-Demand Instances are used.
--
-- Default: @100@
instancesDistribution_onDemandPercentageAboveBaseCapacity :: Lens.Lens' InstancesDistribution (Prelude.Maybe Prelude.Int)
instancesDistribution_onDemandPercentageAboveBaseCapacity = Lens.lens (\InstancesDistribution' {onDemandPercentageAboveBaseCapacity} -> onDemandPercentageAboveBaseCapacity) (\s@InstancesDistribution' {} a -> s {onDemandPercentageAboveBaseCapacity = a} :: InstancesDistribution)

-- | The order of the launch template overrides to use in fulfilling
-- On-Demand capacity.
--
-- If you specify @lowest-price@, Amazon EC2 Auto Scaling uses price to
-- determine the order, launching the lowest price first.
--
-- If you specify @prioritized@, Amazon EC2 Auto Scaling uses the priority
-- that you assigned to each launch template override, launching the
-- highest priority first. If all your On-Demand capacity cannot be
-- fulfilled using your highest priority instance, then Amazon EC2 Auto
-- Scaling launches the remaining capacity using the second priority
-- instance type, and so on.
--
-- Default: @lowest-price@ for Auto Scaling groups that specify
-- InstanceRequirements in the overrides and @prioritized@ for Auto Scaling
-- groups that don\'t.
--
-- Valid values: @lowest-price@ | @prioritized@
instancesDistribution_onDemandAllocationStrategy :: Lens.Lens' InstancesDistribution (Prelude.Maybe Prelude.Text)
instancesDistribution_onDemandAllocationStrategy = Lens.lens (\InstancesDistribution' {onDemandAllocationStrategy} -> onDemandAllocationStrategy) (\s@InstancesDistribution' {} a -> s {onDemandAllocationStrategy = a} :: InstancesDistribution)

-- | The number of Spot Instance pools across which to allocate your Spot
-- Instances. The Spot pools are determined from the different instance
-- types in the overrides. Valid only when the Spot allocation strategy is
-- @lowest-price@. Value must be in the range of 1–20.
--
-- Default: @2@
instancesDistribution_spotInstancePools :: Lens.Lens' InstancesDistribution (Prelude.Maybe Prelude.Int)
instancesDistribution_spotInstancePools = Lens.lens (\InstancesDistribution' {spotInstancePools} -> spotInstancePools) (\s@InstancesDistribution' {} a -> s {spotInstancePools = a} :: InstancesDistribution)

-- | The maximum price per unit hour that you are willing to pay for a Spot
-- Instance. If you keep the value at its default (unspecified), Amazon EC2
-- Auto Scaling uses the On-Demand price as the maximum Spot price. To
-- remove a value that you previously set, include the property but specify
-- an empty string (\"\") for the value.
--
-- If your maximum price is lower than the Spot price for the instance
-- types that you selected, your Spot Instances are not launched.
--
-- Valid Range: Minimum value of 0.001
instancesDistribution_spotMaxPrice :: Lens.Lens' InstancesDistribution (Prelude.Maybe Prelude.Text)
instancesDistribution_spotMaxPrice = Lens.lens (\InstancesDistribution' {spotMaxPrice} -> spotMaxPrice) (\s@InstancesDistribution' {} a -> s {spotMaxPrice = a} :: InstancesDistribution)

-- | Indicates how to allocate instances across Spot Instance pools.
--
-- If the allocation strategy is @lowest-price@, the Auto Scaling group
-- launches instances using the Spot pools with the lowest price, and
-- evenly allocates your instances across the number of Spot pools that you
-- specify.
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
-- Default: @lowest-price@
--
-- Valid values: @lowest-price@ | @capacity-optimized@ |
-- @capacity-optimized-prioritized@
instancesDistribution_spotAllocationStrategy :: Lens.Lens' InstancesDistribution (Prelude.Maybe Prelude.Text)
instancesDistribution_spotAllocationStrategy = Lens.lens (\InstancesDistribution' {spotAllocationStrategy} -> spotAllocationStrategy) (\s@InstancesDistribution' {} a -> s {spotAllocationStrategy = a} :: InstancesDistribution)

instance Core.FromXML InstancesDistribution where
  parseXML x =
    InstancesDistribution'
      Prelude.<$> (x Core..@? "OnDemandBaseCapacity")
      Prelude.<*> (x Core..@? "OnDemandPercentageAboveBaseCapacity")
      Prelude.<*> (x Core..@? "OnDemandAllocationStrategy")
      Prelude.<*> (x Core..@? "SpotInstancePools")
      Prelude.<*> (x Core..@? "SpotMaxPrice")
      Prelude.<*> (x Core..@? "SpotAllocationStrategy")

instance Prelude.Hashable InstancesDistribution where
  hashWithSalt _salt InstancesDistribution' {..} =
    _salt `Prelude.hashWithSalt` onDemandBaseCapacity
      `Prelude.hashWithSalt` onDemandPercentageAboveBaseCapacity
      `Prelude.hashWithSalt` onDemandAllocationStrategy
      `Prelude.hashWithSalt` spotInstancePools
      `Prelude.hashWithSalt` spotMaxPrice
      `Prelude.hashWithSalt` spotAllocationStrategy

instance Prelude.NFData InstancesDistribution where
  rnf InstancesDistribution' {..} =
    Prelude.rnf onDemandBaseCapacity
      `Prelude.seq` Prelude.rnf onDemandPercentageAboveBaseCapacity
      `Prelude.seq` Prelude.rnf onDemandAllocationStrategy
      `Prelude.seq` Prelude.rnf spotInstancePools
      `Prelude.seq` Prelude.rnf spotMaxPrice
      `Prelude.seq` Prelude.rnf spotAllocationStrategy

instance Core.ToQuery InstancesDistribution where
  toQuery InstancesDistribution' {..} =
    Prelude.mconcat
      [ "OnDemandBaseCapacity" Core.=: onDemandBaseCapacity,
        "OnDemandPercentageAboveBaseCapacity"
          Core.=: onDemandPercentageAboveBaseCapacity,
        "OnDemandAllocationStrategy"
          Core.=: onDemandAllocationStrategy,
        "SpotInstancePools" Core.=: spotInstancePools,
        "SpotMaxPrice" Core.=: spotMaxPrice,
        "SpotAllocationStrategy"
          Core.=: spotAllocationStrategy
      ]
