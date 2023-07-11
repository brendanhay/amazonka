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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.InstancesDistribution where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Use this structure to specify the distribution of On-Demand Instances
-- and Spot Instances and the allocation strategies used to fulfill
-- On-Demand and Spot capacities for a mixed instances policy.
--
-- /See:/ 'newInstancesDistribution' smart constructor.
data InstancesDistribution = InstancesDistribution'
  { -- | The allocation strategy to apply to your On-Demand Instances when they
    -- are launched. Possible instance types are determined by the launch
    -- template overrides that you specify.
    --
    -- The following lists the valid values:
    --
    -- [lowest-price]
    --     Uses price to determine which instance types are the highest
    --     priority, launching the lowest priced instance types within an
    --     Availability Zone first. This is the default value for Auto Scaling
    --     groups that specify InstanceRequirements.
    --
    -- [prioritized]
    --     You set the order of instance types for the launch template
    --     overrides from highest to lowest priority (from first to last in the
    --     list). Amazon EC2 Auto Scaling launches your highest priority
    --     instance types first. If all your On-Demand capacity cannot be
    --     fulfilled using your highest priority instance type, then Amazon EC2
    --     Auto Scaling launches the remaining capacity using the second
    --     priority instance type, and so on. This is the default value for
    --     Auto Scaling groups that don\'t specify InstanceRequirements and
    --     cannot be used for groups that do.
    onDemandAllocationStrategy :: Prelude.Maybe Prelude.Text,
    -- | The minimum amount of the Auto Scaling group\'s capacity that must be
    -- fulfilled by On-Demand Instances. This base portion is launched first as
    -- your group scales.
    --
    -- This number has the same unit of measurement as the group\'s desired
    -- capacity. If you change the default unit of measurement (number of
    -- instances) by specifying weighted capacity values in your launch
    -- template overrides list, or by changing the default desired capacity
    -- type setting of the group, you must specify this number using the same
    -- unit of measurement.
    --
    -- Default: 0
    onDemandBaseCapacity :: Prelude.Maybe Prelude.Int,
    -- | Controls the percentages of On-Demand Instances and Spot Instances for
    -- your additional capacity beyond @OnDemandBaseCapacity@. Expressed as a
    -- number (for example, 20 specifies 20% On-Demand Instances, 80% Spot
    -- Instances). If set to 100, only On-Demand Instances are used.
    --
    -- Default: 100
    onDemandPercentageAboveBaseCapacity :: Prelude.Maybe Prelude.Int,
    -- | The allocation strategy to apply to your Spot Instances when they are
    -- launched. Possible instance types are determined by the launch template
    -- overrides that you specify.
    --
    -- The following lists the valid values:
    --
    -- [capacity-optimized]
    --     Requests Spot Instances using pools that are optimally chosen based
    --     on the available Spot capacity. This strategy has the lowest risk of
    --     interruption. To give certain instance types a higher chance of
    --     launching first, use @capacity-optimized-prioritized@.
    --
    -- [capacity-optimized-prioritized]
    --     You set the order of instance types for the launch template
    --     overrides from highest to lowest priority (from first to last in the
    --     list). Amazon EC2 Auto Scaling honors the instance type priorities
    --     on a best effort basis but optimizes for capacity first. Note that
    --     if the On-Demand allocation strategy is set to @prioritized@, the
    --     same priority is applied when fulfilling On-Demand capacity. This is
    --     not a valid value for Auto Scaling groups that specify
    --     InstanceRequirements.
    --
    -- [lowest-price]
    --     Requests Spot Instances using the lowest priced pools within an
    --     Availability Zone, across the number of Spot pools that you specify
    --     for the @SpotInstancePools@ property. To ensure that your desired
    --     capacity is met, you might receive Spot Instances from several
    --     pools. This is the default value, but it might lead to high
    --     interruption rates because this strategy only considers instance
    --     price and not available capacity.
    --
    -- [price-capacity-optimized (recommended)]
    --     The price and capacity optimized allocation strategy looks at both
    --     price and capacity to select the Spot Instance pools that are the
    --     least likely to be interrupted and have the lowest possible price.
    spotAllocationStrategy :: Prelude.Maybe Prelude.Text,
    -- | The number of Spot Instance pools across which to allocate your Spot
    -- Instances. The Spot pools are determined from the different instance
    -- types in the overrides. Valid only when the @SpotAllocationStrategy@ is
    -- @lowest-price@. Value must be in the range of 1–20.
    --
    -- Default: 2
    spotInstancePools :: Prelude.Maybe Prelude.Int,
    -- | The maximum price per unit hour that you are willing to pay for a Spot
    -- Instance. If your maximum price is lower than the Spot price for the
    -- instance types that you selected, your Spot Instances are not launched.
    -- We do not recommend specifying a maximum price because it can lead to
    -- increased interruptions. When Spot Instances launch, you pay the current
    -- Spot price. To remove a maximum price that you previously set, include
    -- the property but specify an empty string (\"\") for the value.
    --
    -- If you specify a maximum price, your instances will be interrupted more
    -- frequently than if you do not specify one.
    --
    -- Valid Range: Minimum value of 0.001
    spotMaxPrice :: Prelude.Maybe Prelude.Text
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
-- 'onDemandAllocationStrategy', 'instancesDistribution_onDemandAllocationStrategy' - The allocation strategy to apply to your On-Demand Instances when they
-- are launched. Possible instance types are determined by the launch
-- template overrides that you specify.
--
-- The following lists the valid values:
--
-- [lowest-price]
--     Uses price to determine which instance types are the highest
--     priority, launching the lowest priced instance types within an
--     Availability Zone first. This is the default value for Auto Scaling
--     groups that specify InstanceRequirements.
--
-- [prioritized]
--     You set the order of instance types for the launch template
--     overrides from highest to lowest priority (from first to last in the
--     list). Amazon EC2 Auto Scaling launches your highest priority
--     instance types first. If all your On-Demand capacity cannot be
--     fulfilled using your highest priority instance type, then Amazon EC2
--     Auto Scaling launches the remaining capacity using the second
--     priority instance type, and so on. This is the default value for
--     Auto Scaling groups that don\'t specify InstanceRequirements and
--     cannot be used for groups that do.
--
-- 'onDemandBaseCapacity', 'instancesDistribution_onDemandBaseCapacity' - The minimum amount of the Auto Scaling group\'s capacity that must be
-- fulfilled by On-Demand Instances. This base portion is launched first as
-- your group scales.
--
-- This number has the same unit of measurement as the group\'s desired
-- capacity. If you change the default unit of measurement (number of
-- instances) by specifying weighted capacity values in your launch
-- template overrides list, or by changing the default desired capacity
-- type setting of the group, you must specify this number using the same
-- unit of measurement.
--
-- Default: 0
--
-- 'onDemandPercentageAboveBaseCapacity', 'instancesDistribution_onDemandPercentageAboveBaseCapacity' - Controls the percentages of On-Demand Instances and Spot Instances for
-- your additional capacity beyond @OnDemandBaseCapacity@. Expressed as a
-- number (for example, 20 specifies 20% On-Demand Instances, 80% Spot
-- Instances). If set to 100, only On-Demand Instances are used.
--
-- Default: 100
--
-- 'spotAllocationStrategy', 'instancesDistribution_spotAllocationStrategy' - The allocation strategy to apply to your Spot Instances when they are
-- launched. Possible instance types are determined by the launch template
-- overrides that you specify.
--
-- The following lists the valid values:
--
-- [capacity-optimized]
--     Requests Spot Instances using pools that are optimally chosen based
--     on the available Spot capacity. This strategy has the lowest risk of
--     interruption. To give certain instance types a higher chance of
--     launching first, use @capacity-optimized-prioritized@.
--
-- [capacity-optimized-prioritized]
--     You set the order of instance types for the launch template
--     overrides from highest to lowest priority (from first to last in the
--     list). Amazon EC2 Auto Scaling honors the instance type priorities
--     on a best effort basis but optimizes for capacity first. Note that
--     if the On-Demand allocation strategy is set to @prioritized@, the
--     same priority is applied when fulfilling On-Demand capacity. This is
--     not a valid value for Auto Scaling groups that specify
--     InstanceRequirements.
--
-- [lowest-price]
--     Requests Spot Instances using the lowest priced pools within an
--     Availability Zone, across the number of Spot pools that you specify
--     for the @SpotInstancePools@ property. To ensure that your desired
--     capacity is met, you might receive Spot Instances from several
--     pools. This is the default value, but it might lead to high
--     interruption rates because this strategy only considers instance
--     price and not available capacity.
--
-- [price-capacity-optimized (recommended)]
--     The price and capacity optimized allocation strategy looks at both
--     price and capacity to select the Spot Instance pools that are the
--     least likely to be interrupted and have the lowest possible price.
--
-- 'spotInstancePools', 'instancesDistribution_spotInstancePools' - The number of Spot Instance pools across which to allocate your Spot
-- Instances. The Spot pools are determined from the different instance
-- types in the overrides. Valid only when the @SpotAllocationStrategy@ is
-- @lowest-price@. Value must be in the range of 1–20.
--
-- Default: 2
--
-- 'spotMaxPrice', 'instancesDistribution_spotMaxPrice' - The maximum price per unit hour that you are willing to pay for a Spot
-- Instance. If your maximum price is lower than the Spot price for the
-- instance types that you selected, your Spot Instances are not launched.
-- We do not recommend specifying a maximum price because it can lead to
-- increased interruptions. When Spot Instances launch, you pay the current
-- Spot price. To remove a maximum price that you previously set, include
-- the property but specify an empty string (\"\") for the value.
--
-- If you specify a maximum price, your instances will be interrupted more
-- frequently than if you do not specify one.
--
-- Valid Range: Minimum value of 0.001
newInstancesDistribution ::
  InstancesDistribution
newInstancesDistribution =
  InstancesDistribution'
    { onDemandAllocationStrategy =
        Prelude.Nothing,
      onDemandBaseCapacity = Prelude.Nothing,
      onDemandPercentageAboveBaseCapacity =
        Prelude.Nothing,
      spotAllocationStrategy = Prelude.Nothing,
      spotInstancePools = Prelude.Nothing,
      spotMaxPrice = Prelude.Nothing
    }

-- | The allocation strategy to apply to your On-Demand Instances when they
-- are launched. Possible instance types are determined by the launch
-- template overrides that you specify.
--
-- The following lists the valid values:
--
-- [lowest-price]
--     Uses price to determine which instance types are the highest
--     priority, launching the lowest priced instance types within an
--     Availability Zone first. This is the default value for Auto Scaling
--     groups that specify InstanceRequirements.
--
-- [prioritized]
--     You set the order of instance types for the launch template
--     overrides from highest to lowest priority (from first to last in the
--     list). Amazon EC2 Auto Scaling launches your highest priority
--     instance types first. If all your On-Demand capacity cannot be
--     fulfilled using your highest priority instance type, then Amazon EC2
--     Auto Scaling launches the remaining capacity using the second
--     priority instance type, and so on. This is the default value for
--     Auto Scaling groups that don\'t specify InstanceRequirements and
--     cannot be used for groups that do.
instancesDistribution_onDemandAllocationStrategy :: Lens.Lens' InstancesDistribution (Prelude.Maybe Prelude.Text)
instancesDistribution_onDemandAllocationStrategy = Lens.lens (\InstancesDistribution' {onDemandAllocationStrategy} -> onDemandAllocationStrategy) (\s@InstancesDistribution' {} a -> s {onDemandAllocationStrategy = a} :: InstancesDistribution)

-- | The minimum amount of the Auto Scaling group\'s capacity that must be
-- fulfilled by On-Demand Instances. This base portion is launched first as
-- your group scales.
--
-- This number has the same unit of measurement as the group\'s desired
-- capacity. If you change the default unit of measurement (number of
-- instances) by specifying weighted capacity values in your launch
-- template overrides list, or by changing the default desired capacity
-- type setting of the group, you must specify this number using the same
-- unit of measurement.
--
-- Default: 0
instancesDistribution_onDemandBaseCapacity :: Lens.Lens' InstancesDistribution (Prelude.Maybe Prelude.Int)
instancesDistribution_onDemandBaseCapacity = Lens.lens (\InstancesDistribution' {onDemandBaseCapacity} -> onDemandBaseCapacity) (\s@InstancesDistribution' {} a -> s {onDemandBaseCapacity = a} :: InstancesDistribution)

-- | Controls the percentages of On-Demand Instances and Spot Instances for
-- your additional capacity beyond @OnDemandBaseCapacity@. Expressed as a
-- number (for example, 20 specifies 20% On-Demand Instances, 80% Spot
-- Instances). If set to 100, only On-Demand Instances are used.
--
-- Default: 100
instancesDistribution_onDemandPercentageAboveBaseCapacity :: Lens.Lens' InstancesDistribution (Prelude.Maybe Prelude.Int)
instancesDistribution_onDemandPercentageAboveBaseCapacity = Lens.lens (\InstancesDistribution' {onDemandPercentageAboveBaseCapacity} -> onDemandPercentageAboveBaseCapacity) (\s@InstancesDistribution' {} a -> s {onDemandPercentageAboveBaseCapacity = a} :: InstancesDistribution)

-- | The allocation strategy to apply to your Spot Instances when they are
-- launched. Possible instance types are determined by the launch template
-- overrides that you specify.
--
-- The following lists the valid values:
--
-- [capacity-optimized]
--     Requests Spot Instances using pools that are optimally chosen based
--     on the available Spot capacity. This strategy has the lowest risk of
--     interruption. To give certain instance types a higher chance of
--     launching first, use @capacity-optimized-prioritized@.
--
-- [capacity-optimized-prioritized]
--     You set the order of instance types for the launch template
--     overrides from highest to lowest priority (from first to last in the
--     list). Amazon EC2 Auto Scaling honors the instance type priorities
--     on a best effort basis but optimizes for capacity first. Note that
--     if the On-Demand allocation strategy is set to @prioritized@, the
--     same priority is applied when fulfilling On-Demand capacity. This is
--     not a valid value for Auto Scaling groups that specify
--     InstanceRequirements.
--
-- [lowest-price]
--     Requests Spot Instances using the lowest priced pools within an
--     Availability Zone, across the number of Spot pools that you specify
--     for the @SpotInstancePools@ property. To ensure that your desired
--     capacity is met, you might receive Spot Instances from several
--     pools. This is the default value, but it might lead to high
--     interruption rates because this strategy only considers instance
--     price and not available capacity.
--
-- [price-capacity-optimized (recommended)]
--     The price and capacity optimized allocation strategy looks at both
--     price and capacity to select the Spot Instance pools that are the
--     least likely to be interrupted and have the lowest possible price.
instancesDistribution_spotAllocationStrategy :: Lens.Lens' InstancesDistribution (Prelude.Maybe Prelude.Text)
instancesDistribution_spotAllocationStrategy = Lens.lens (\InstancesDistribution' {spotAllocationStrategy} -> spotAllocationStrategy) (\s@InstancesDistribution' {} a -> s {spotAllocationStrategy = a} :: InstancesDistribution)

-- | The number of Spot Instance pools across which to allocate your Spot
-- Instances. The Spot pools are determined from the different instance
-- types in the overrides. Valid only when the @SpotAllocationStrategy@ is
-- @lowest-price@. Value must be in the range of 1–20.
--
-- Default: 2
instancesDistribution_spotInstancePools :: Lens.Lens' InstancesDistribution (Prelude.Maybe Prelude.Int)
instancesDistribution_spotInstancePools = Lens.lens (\InstancesDistribution' {spotInstancePools} -> spotInstancePools) (\s@InstancesDistribution' {} a -> s {spotInstancePools = a} :: InstancesDistribution)

-- | The maximum price per unit hour that you are willing to pay for a Spot
-- Instance. If your maximum price is lower than the Spot price for the
-- instance types that you selected, your Spot Instances are not launched.
-- We do not recommend specifying a maximum price because it can lead to
-- increased interruptions. When Spot Instances launch, you pay the current
-- Spot price. To remove a maximum price that you previously set, include
-- the property but specify an empty string (\"\") for the value.
--
-- If you specify a maximum price, your instances will be interrupted more
-- frequently than if you do not specify one.
--
-- Valid Range: Minimum value of 0.001
instancesDistribution_spotMaxPrice :: Lens.Lens' InstancesDistribution (Prelude.Maybe Prelude.Text)
instancesDistribution_spotMaxPrice = Lens.lens (\InstancesDistribution' {spotMaxPrice} -> spotMaxPrice) (\s@InstancesDistribution' {} a -> s {spotMaxPrice = a} :: InstancesDistribution)

instance Data.FromXML InstancesDistribution where
  parseXML x =
    InstancesDistribution'
      Prelude.<$> (x Data..@? "OnDemandAllocationStrategy")
      Prelude.<*> (x Data..@? "OnDemandBaseCapacity")
      Prelude.<*> (x Data..@? "OnDemandPercentageAboveBaseCapacity")
      Prelude.<*> (x Data..@? "SpotAllocationStrategy")
      Prelude.<*> (x Data..@? "SpotInstancePools")
      Prelude.<*> (x Data..@? "SpotMaxPrice")

instance Prelude.Hashable InstancesDistribution where
  hashWithSalt _salt InstancesDistribution' {..} =
    _salt
      `Prelude.hashWithSalt` onDemandAllocationStrategy
      `Prelude.hashWithSalt` onDemandBaseCapacity
      `Prelude.hashWithSalt` onDemandPercentageAboveBaseCapacity
      `Prelude.hashWithSalt` spotAllocationStrategy
      `Prelude.hashWithSalt` spotInstancePools
      `Prelude.hashWithSalt` spotMaxPrice

instance Prelude.NFData InstancesDistribution where
  rnf InstancesDistribution' {..} =
    Prelude.rnf onDemandAllocationStrategy
      `Prelude.seq` Prelude.rnf onDemandBaseCapacity
      `Prelude.seq` Prelude.rnf onDemandPercentageAboveBaseCapacity
      `Prelude.seq` Prelude.rnf spotAllocationStrategy
      `Prelude.seq` Prelude.rnf spotInstancePools
      `Prelude.seq` Prelude.rnf spotMaxPrice

instance Data.ToQuery InstancesDistribution where
  toQuery InstancesDistribution' {..} =
    Prelude.mconcat
      [ "OnDemandAllocationStrategy"
          Data.=: onDemandAllocationStrategy,
        "OnDemandBaseCapacity" Data.=: onDemandBaseCapacity,
        "OnDemandPercentageAboveBaseCapacity"
          Data.=: onDemandPercentageAboveBaseCapacity,
        "SpotAllocationStrategy"
          Data.=: spotAllocationStrategy,
        "SpotInstancePools" Data.=: spotInstancePools,
        "SpotMaxPrice" Data.=: spotMaxPrice
      ]
