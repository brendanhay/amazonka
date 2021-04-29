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
-- Module      : Network.AWS.AutoScaling.Types.InstancesDistribution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScaling.Types.InstancesDistribution where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an instances distribution for an Auto Scaling group with a
-- MixedInstancesPolicy.
--
-- The instances distribution specifies the distribution of On-Demand
-- Instances and Spot Instances, the maximum price to pay for Spot
-- Instances, and how the Auto Scaling group allocates instance types to
-- fulfill On-Demand and Spot capacities.
--
-- When you update @SpotAllocationStrategy@, @SpotInstancePools@, or
-- @SpotMaxPrice@, this update action does not deploy any changes across
-- the running Amazon EC2 instances in the group. Your existing Spot
-- Instances continue to run as long as the maximum price for those
-- instances is higher than the current Spot price. When scale out occurs,
-- Amazon EC2 Auto Scaling launches instances based on the new settings.
-- When scale in occurs, Amazon EC2 Auto Scaling terminates instances
-- according to the group\'s termination policies.
--
-- /See:/ 'newInstancesDistribution' smart constructor.
data InstancesDistribution = InstancesDistribution'
  { -- | The maximum price per unit hour that you are willing to pay for a Spot
    -- Instance. If you leave the value at its default (empty), Amazon EC2 Auto
    -- Scaling uses the On-Demand price as the maximum Spot price. To remove a
    -- value that you previously set, include the property but specify an empty
    -- string (\"\") for the value.
    spotMaxPrice :: Prelude.Maybe Prelude.Text,
    -- | The number of Spot Instance pools across which to allocate your Spot
    -- Instances. The Spot pools are determined from the different instance
    -- types in the overrides. Valid only when the Spot allocation strategy is
    -- @lowest-price@. Value must be in the range of 1 to 20. Defaults to 2 if
    -- not specified.
    spotInstancePools :: Prelude.Maybe Prelude.Int,
    -- | Indicates how to allocate instances across Spot Instance pools. If the
    -- allocation strategy is @capacity-optimized@ (recommended), the Auto
    -- Scaling group launches instances using Spot pools that are optimally
    -- chosen based on the available Spot capacity. If the allocation strategy
    -- is @lowest-price@, the Auto Scaling group launches instances using the
    -- Spot pools with the lowest price, and evenly allocates your instances
    -- across the number of Spot pools that you specify. Defaults to
    -- @lowest-price@ if not specified.
    spotAllocationStrategy :: Prelude.Maybe Prelude.Text,
    -- | Controls the percentages of On-Demand Instances and Spot Instances for
    -- your additional capacity beyond @OnDemandBaseCapacity@. Expressed as a
    -- number (for example, 20 specifies 20% On-Demand Instances, 80% Spot
    -- Instances). Defaults to 100 if not specified. If set to 100, only
    -- On-Demand Instances are provisioned.
    onDemandPercentageAboveBaseCapacity :: Prelude.Maybe Prelude.Int,
    -- | Indicates how to allocate instance types to fulfill On-Demand capacity.
    -- The only valid value is @prioritized@, which is also the default value.
    -- This strategy uses the order of instance types in the overrides to
    -- define the launch priority of each instance type. The first instance
    -- type in the array is prioritized higher than the last. If all your
    -- On-Demand capacity cannot be fulfilled using your highest priority
    -- instance, then the Auto Scaling groups launches the remaining capacity
    -- using the second priority instance type, and so on.
    onDemandAllocationStrategy :: Prelude.Maybe Prelude.Text,
    -- | The minimum amount of the Auto Scaling group\'s capacity that must be
    -- fulfilled by On-Demand Instances. This base portion is provisioned first
    -- as your group scales. Defaults to 0 if not specified. If you specify
    -- weights for the instance types in the overrides, set the value of
    -- @OnDemandBaseCapacity@ in terms of the number of capacity units, and not
    -- the number of instances.
    onDemandBaseCapacity :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InstancesDistribution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'spotMaxPrice', 'instancesDistribution_spotMaxPrice' - The maximum price per unit hour that you are willing to pay for a Spot
-- Instance. If you leave the value at its default (empty), Amazon EC2 Auto
-- Scaling uses the On-Demand price as the maximum Spot price. To remove a
-- value that you previously set, include the property but specify an empty
-- string (\"\") for the value.
--
-- 'spotInstancePools', 'instancesDistribution_spotInstancePools' - The number of Spot Instance pools across which to allocate your Spot
-- Instances. The Spot pools are determined from the different instance
-- types in the overrides. Valid only when the Spot allocation strategy is
-- @lowest-price@. Value must be in the range of 1 to 20. Defaults to 2 if
-- not specified.
--
-- 'spotAllocationStrategy', 'instancesDistribution_spotAllocationStrategy' - Indicates how to allocate instances across Spot Instance pools. If the
-- allocation strategy is @capacity-optimized@ (recommended), the Auto
-- Scaling group launches instances using Spot pools that are optimally
-- chosen based on the available Spot capacity. If the allocation strategy
-- is @lowest-price@, the Auto Scaling group launches instances using the
-- Spot pools with the lowest price, and evenly allocates your instances
-- across the number of Spot pools that you specify. Defaults to
-- @lowest-price@ if not specified.
--
-- 'onDemandPercentageAboveBaseCapacity', 'instancesDistribution_onDemandPercentageAboveBaseCapacity' - Controls the percentages of On-Demand Instances and Spot Instances for
-- your additional capacity beyond @OnDemandBaseCapacity@. Expressed as a
-- number (for example, 20 specifies 20% On-Demand Instances, 80% Spot
-- Instances). Defaults to 100 if not specified. If set to 100, only
-- On-Demand Instances are provisioned.
--
-- 'onDemandAllocationStrategy', 'instancesDistribution_onDemandAllocationStrategy' - Indicates how to allocate instance types to fulfill On-Demand capacity.
-- The only valid value is @prioritized@, which is also the default value.
-- This strategy uses the order of instance types in the overrides to
-- define the launch priority of each instance type. The first instance
-- type in the array is prioritized higher than the last. If all your
-- On-Demand capacity cannot be fulfilled using your highest priority
-- instance, then the Auto Scaling groups launches the remaining capacity
-- using the second priority instance type, and so on.
--
-- 'onDemandBaseCapacity', 'instancesDistribution_onDemandBaseCapacity' - The minimum amount of the Auto Scaling group\'s capacity that must be
-- fulfilled by On-Demand Instances. This base portion is provisioned first
-- as your group scales. Defaults to 0 if not specified. If you specify
-- weights for the instance types in the overrides, set the value of
-- @OnDemandBaseCapacity@ in terms of the number of capacity units, and not
-- the number of instances.
newInstancesDistribution ::
  InstancesDistribution
newInstancesDistribution =
  InstancesDistribution'
    { spotMaxPrice =
        Prelude.Nothing,
      spotInstancePools = Prelude.Nothing,
      spotAllocationStrategy = Prelude.Nothing,
      onDemandPercentageAboveBaseCapacity =
        Prelude.Nothing,
      onDemandAllocationStrategy = Prelude.Nothing,
      onDemandBaseCapacity = Prelude.Nothing
    }

-- | The maximum price per unit hour that you are willing to pay for a Spot
-- Instance. If you leave the value at its default (empty), Amazon EC2 Auto
-- Scaling uses the On-Demand price as the maximum Spot price. To remove a
-- value that you previously set, include the property but specify an empty
-- string (\"\") for the value.
instancesDistribution_spotMaxPrice :: Lens.Lens' InstancesDistribution (Prelude.Maybe Prelude.Text)
instancesDistribution_spotMaxPrice = Lens.lens (\InstancesDistribution' {spotMaxPrice} -> spotMaxPrice) (\s@InstancesDistribution' {} a -> s {spotMaxPrice = a} :: InstancesDistribution)

-- | The number of Spot Instance pools across which to allocate your Spot
-- Instances. The Spot pools are determined from the different instance
-- types in the overrides. Valid only when the Spot allocation strategy is
-- @lowest-price@. Value must be in the range of 1 to 20. Defaults to 2 if
-- not specified.
instancesDistribution_spotInstancePools :: Lens.Lens' InstancesDistribution (Prelude.Maybe Prelude.Int)
instancesDistribution_spotInstancePools = Lens.lens (\InstancesDistribution' {spotInstancePools} -> spotInstancePools) (\s@InstancesDistribution' {} a -> s {spotInstancePools = a} :: InstancesDistribution)

-- | Indicates how to allocate instances across Spot Instance pools. If the
-- allocation strategy is @capacity-optimized@ (recommended), the Auto
-- Scaling group launches instances using Spot pools that are optimally
-- chosen based on the available Spot capacity. If the allocation strategy
-- is @lowest-price@, the Auto Scaling group launches instances using the
-- Spot pools with the lowest price, and evenly allocates your instances
-- across the number of Spot pools that you specify. Defaults to
-- @lowest-price@ if not specified.
instancesDistribution_spotAllocationStrategy :: Lens.Lens' InstancesDistribution (Prelude.Maybe Prelude.Text)
instancesDistribution_spotAllocationStrategy = Lens.lens (\InstancesDistribution' {spotAllocationStrategy} -> spotAllocationStrategy) (\s@InstancesDistribution' {} a -> s {spotAllocationStrategy = a} :: InstancesDistribution)

-- | Controls the percentages of On-Demand Instances and Spot Instances for
-- your additional capacity beyond @OnDemandBaseCapacity@. Expressed as a
-- number (for example, 20 specifies 20% On-Demand Instances, 80% Spot
-- Instances). Defaults to 100 if not specified. If set to 100, only
-- On-Demand Instances are provisioned.
instancesDistribution_onDemandPercentageAboveBaseCapacity :: Lens.Lens' InstancesDistribution (Prelude.Maybe Prelude.Int)
instancesDistribution_onDemandPercentageAboveBaseCapacity = Lens.lens (\InstancesDistribution' {onDemandPercentageAboveBaseCapacity} -> onDemandPercentageAboveBaseCapacity) (\s@InstancesDistribution' {} a -> s {onDemandPercentageAboveBaseCapacity = a} :: InstancesDistribution)

-- | Indicates how to allocate instance types to fulfill On-Demand capacity.
-- The only valid value is @prioritized@, which is also the default value.
-- This strategy uses the order of instance types in the overrides to
-- define the launch priority of each instance type. The first instance
-- type in the array is prioritized higher than the last. If all your
-- On-Demand capacity cannot be fulfilled using your highest priority
-- instance, then the Auto Scaling groups launches the remaining capacity
-- using the second priority instance type, and so on.
instancesDistribution_onDemandAllocationStrategy :: Lens.Lens' InstancesDistribution (Prelude.Maybe Prelude.Text)
instancesDistribution_onDemandAllocationStrategy = Lens.lens (\InstancesDistribution' {onDemandAllocationStrategy} -> onDemandAllocationStrategy) (\s@InstancesDistribution' {} a -> s {onDemandAllocationStrategy = a} :: InstancesDistribution)

-- | The minimum amount of the Auto Scaling group\'s capacity that must be
-- fulfilled by On-Demand Instances. This base portion is provisioned first
-- as your group scales. Defaults to 0 if not specified. If you specify
-- weights for the instance types in the overrides, set the value of
-- @OnDemandBaseCapacity@ in terms of the number of capacity units, and not
-- the number of instances.
instancesDistribution_onDemandBaseCapacity :: Lens.Lens' InstancesDistribution (Prelude.Maybe Prelude.Int)
instancesDistribution_onDemandBaseCapacity = Lens.lens (\InstancesDistribution' {onDemandBaseCapacity} -> onDemandBaseCapacity) (\s@InstancesDistribution' {} a -> s {onDemandBaseCapacity = a} :: InstancesDistribution)

instance Prelude.FromXML InstancesDistribution where
  parseXML x =
    InstancesDistribution'
      Prelude.<$> (x Prelude..@? "SpotMaxPrice")
      Prelude.<*> (x Prelude..@? "SpotInstancePools")
      Prelude.<*> (x Prelude..@? "SpotAllocationStrategy")
      Prelude.<*> (x Prelude..@? "OnDemandPercentageAboveBaseCapacity")
      Prelude.<*> (x Prelude..@? "OnDemandAllocationStrategy")
      Prelude.<*> (x Prelude..@? "OnDemandBaseCapacity")

instance Prelude.Hashable InstancesDistribution

instance Prelude.NFData InstancesDistribution

instance Prelude.ToQuery InstancesDistribution where
  toQuery InstancesDistribution' {..} =
    Prelude.mconcat
      [ "SpotMaxPrice" Prelude.=: spotMaxPrice,
        "SpotInstancePools" Prelude.=: spotInstancePools,
        "SpotAllocationStrategy"
          Prelude.=: spotAllocationStrategy,
        "OnDemandPercentageAboveBaseCapacity"
          Prelude.=: onDemandPercentageAboveBaseCapacity,
        "OnDemandAllocationStrategy"
          Prelude.=: onDemandAllocationStrategy,
        "OnDemandBaseCapacity"
          Prelude.=: onDemandBaseCapacity
      ]
