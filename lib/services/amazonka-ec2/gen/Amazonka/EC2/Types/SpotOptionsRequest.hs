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
-- Module      : Amazonka.EC2.Types.SpotOptionsRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.SpotOptionsRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.FleetSpotMaintenanceStrategiesRequest
import Amazonka.EC2.Types.SpotAllocationStrategy
import Amazonka.EC2.Types.SpotInstanceInterruptionBehavior
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration of Spot Instances in an EC2 Fleet request.
--
-- /See:/ 'newSpotOptionsRequest' smart constructor.
data SpotOptionsRequest = SpotOptionsRequest'
  { -- | The strategy that determines how to allocate the target Spot Instance
    -- capacity across the Spot Instance pools specified by the EC2 Fleet
    -- launch configuration. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-fleet-allocation-strategy.html Allocation strategies for Spot Instances>
    -- in the /Amazon EC2 User Guide/.
    --
    -- [price-capacity-optimized (recommended)]
    --     EC2 Fleet identifies the pools with the highest capacity
    --     availability for the number of instances that are launching. This
    --     means that we will request Spot Instances from the pools that we
    --     believe have the lowest chance of interruption in the near term. EC2
    --     Fleet then requests Spot Instances from the lowest priced of these
    --     pools.
    --
    -- [capacity-optimized]
    --     EC2 Fleet identifies the pools with the highest capacity
    --     availability for the number of instances that are launching. This
    --     means that we will request Spot Instances from the pools that we
    --     believe have the lowest chance of interruption in the near term. To
    --     give certain instance types a higher chance of launching first, use
    --     @capacity-optimized-prioritized@. Set a priority for each instance
    --     type by using the @Priority@ parameter for
    --     @LaunchTemplateOverrides@. You can assign the same priority to
    --     different @LaunchTemplateOverrides@. EC2 implements the priorities
    --     on a best-effort basis, but optimizes for capacity first.
    --     @capacity-optimized-prioritized@ is supported only if your EC2 Fleet
    --     uses a launch template. Note that if the On-Demand
    --     @AllocationStrategy@ is set to @prioritized@, the same priority is
    --     applied when fulfilling On-Demand capacity.
    --
    -- [diversified]
    --     EC2 Fleet requests instances from all of the Spot Instance pools
    --     that you specify.
    --
    -- [lowest-price]
    --     EC2 Fleet requests instances from the lowest priced Spot Instance
    --     pool that has available capacity. If the lowest priced pool doesn\'t
    --     have available capacity, the Spot Instances come from the next
    --     lowest priced pool that has available capacity. If a pool runs out
    --     of capacity before fulfilling your desired capacity, EC2 Fleet will
    --     continue to fulfill your request by drawing from the next lowest
    --     priced pool. To ensure that your desired capacity is met, you might
    --     receive Spot Instances from several pools. Because this strategy
    --     only considers instance price and not capacity availability, it
    --     might lead to high interruption rates.
    --
    -- Default: @lowest-price@
    allocationStrategy :: Prelude.Maybe SpotAllocationStrategy,
    -- | The behavior when a Spot Instance is interrupted.
    --
    -- Default: @terminate@
    instanceInterruptionBehavior :: Prelude.Maybe SpotInstanceInterruptionBehavior,
    -- | The number of Spot pools across which to allocate your target Spot
    -- capacity. Supported only when Spot @AllocationStrategy@ is set to
    -- @lowest-price@. EC2 Fleet selects the cheapest Spot pools and evenly
    -- allocates your target Spot capacity across the number of Spot pools that
    -- you specify.
    --
    -- Note that EC2 Fleet attempts to draw Spot Instances from the number of
    -- pools that you specify on a best effort basis. If a pool runs out of
    -- Spot capacity before fulfilling your target capacity, EC2 Fleet will
    -- continue to fulfill your request by drawing from the next cheapest pool.
    -- To ensure that your target capacity is met, you might receive Spot
    -- Instances from more than the number of pools that you specified.
    -- Similarly, if most of the pools have no Spot capacity, you might receive
    -- your full target capacity from fewer than the number of pools that you
    -- specified.
    instancePoolsToUseCount :: Prelude.Maybe Prelude.Int,
    -- | The strategies for managing your Spot Instances that are at an elevated
    -- risk of being interrupted.
    maintenanceStrategies :: Prelude.Maybe FleetSpotMaintenanceStrategiesRequest,
    -- | The maximum amount per hour for Spot Instances that you\'re willing to
    -- pay. We do not recommend using this parameter because it can lead to
    -- increased interruptions. If you do not specify this parameter, you will
    -- pay the current Spot price.
    --
    -- If you specify a maximum price, your Spot Instances will be interrupted
    -- more frequently than if you do not specify this parameter.
    maxTotalPrice :: Prelude.Maybe Prelude.Text,
    -- | The minimum target capacity for Spot Instances in the fleet. If the
    -- minimum target capacity is not reached, the fleet launches no instances.
    --
    -- Supported only for fleets of type @instant@.
    --
    -- At least one of the following must be specified:
    -- @SingleAvailabilityZone@ | @SingleInstanceType@
    minTargetCapacity :: Prelude.Maybe Prelude.Int,
    -- | Indicates that the fleet launches all Spot Instances into a single
    -- Availability Zone.
    --
    -- Supported only for fleets of type @instant@.
    singleAvailabilityZone :: Prelude.Maybe Prelude.Bool,
    -- | Indicates that the fleet uses a single instance type to launch all Spot
    -- Instances in the fleet.
    --
    -- Supported only for fleets of type @instant@.
    singleInstanceType :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SpotOptionsRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'allocationStrategy', 'spotOptionsRequest_allocationStrategy' - The strategy that determines how to allocate the target Spot Instance
-- capacity across the Spot Instance pools specified by the EC2 Fleet
-- launch configuration. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-fleet-allocation-strategy.html Allocation strategies for Spot Instances>
-- in the /Amazon EC2 User Guide/.
--
-- [price-capacity-optimized (recommended)]
--     EC2 Fleet identifies the pools with the highest capacity
--     availability for the number of instances that are launching. This
--     means that we will request Spot Instances from the pools that we
--     believe have the lowest chance of interruption in the near term. EC2
--     Fleet then requests Spot Instances from the lowest priced of these
--     pools.
--
-- [capacity-optimized]
--     EC2 Fleet identifies the pools with the highest capacity
--     availability for the number of instances that are launching. This
--     means that we will request Spot Instances from the pools that we
--     believe have the lowest chance of interruption in the near term. To
--     give certain instance types a higher chance of launching first, use
--     @capacity-optimized-prioritized@. Set a priority for each instance
--     type by using the @Priority@ parameter for
--     @LaunchTemplateOverrides@. You can assign the same priority to
--     different @LaunchTemplateOverrides@. EC2 implements the priorities
--     on a best-effort basis, but optimizes for capacity first.
--     @capacity-optimized-prioritized@ is supported only if your EC2 Fleet
--     uses a launch template. Note that if the On-Demand
--     @AllocationStrategy@ is set to @prioritized@, the same priority is
--     applied when fulfilling On-Demand capacity.
--
-- [diversified]
--     EC2 Fleet requests instances from all of the Spot Instance pools
--     that you specify.
--
-- [lowest-price]
--     EC2 Fleet requests instances from the lowest priced Spot Instance
--     pool that has available capacity. If the lowest priced pool doesn\'t
--     have available capacity, the Spot Instances come from the next
--     lowest priced pool that has available capacity. If a pool runs out
--     of capacity before fulfilling your desired capacity, EC2 Fleet will
--     continue to fulfill your request by drawing from the next lowest
--     priced pool. To ensure that your desired capacity is met, you might
--     receive Spot Instances from several pools. Because this strategy
--     only considers instance price and not capacity availability, it
--     might lead to high interruption rates.
--
-- Default: @lowest-price@
--
-- 'instanceInterruptionBehavior', 'spotOptionsRequest_instanceInterruptionBehavior' - The behavior when a Spot Instance is interrupted.
--
-- Default: @terminate@
--
-- 'instancePoolsToUseCount', 'spotOptionsRequest_instancePoolsToUseCount' - The number of Spot pools across which to allocate your target Spot
-- capacity. Supported only when Spot @AllocationStrategy@ is set to
-- @lowest-price@. EC2 Fleet selects the cheapest Spot pools and evenly
-- allocates your target Spot capacity across the number of Spot pools that
-- you specify.
--
-- Note that EC2 Fleet attempts to draw Spot Instances from the number of
-- pools that you specify on a best effort basis. If a pool runs out of
-- Spot capacity before fulfilling your target capacity, EC2 Fleet will
-- continue to fulfill your request by drawing from the next cheapest pool.
-- To ensure that your target capacity is met, you might receive Spot
-- Instances from more than the number of pools that you specified.
-- Similarly, if most of the pools have no Spot capacity, you might receive
-- your full target capacity from fewer than the number of pools that you
-- specified.
--
-- 'maintenanceStrategies', 'spotOptionsRequest_maintenanceStrategies' - The strategies for managing your Spot Instances that are at an elevated
-- risk of being interrupted.
--
-- 'maxTotalPrice', 'spotOptionsRequest_maxTotalPrice' - The maximum amount per hour for Spot Instances that you\'re willing to
-- pay. We do not recommend using this parameter because it can lead to
-- increased interruptions. If you do not specify this parameter, you will
-- pay the current Spot price.
--
-- If you specify a maximum price, your Spot Instances will be interrupted
-- more frequently than if you do not specify this parameter.
--
-- 'minTargetCapacity', 'spotOptionsRequest_minTargetCapacity' - The minimum target capacity for Spot Instances in the fleet. If the
-- minimum target capacity is not reached, the fleet launches no instances.
--
-- Supported only for fleets of type @instant@.
--
-- At least one of the following must be specified:
-- @SingleAvailabilityZone@ | @SingleInstanceType@
--
-- 'singleAvailabilityZone', 'spotOptionsRequest_singleAvailabilityZone' - Indicates that the fleet launches all Spot Instances into a single
-- Availability Zone.
--
-- Supported only for fleets of type @instant@.
--
-- 'singleInstanceType', 'spotOptionsRequest_singleInstanceType' - Indicates that the fleet uses a single instance type to launch all Spot
-- Instances in the fleet.
--
-- Supported only for fleets of type @instant@.
newSpotOptionsRequest ::
  SpotOptionsRequest
newSpotOptionsRequest =
  SpotOptionsRequest'
    { allocationStrategy =
        Prelude.Nothing,
      instanceInterruptionBehavior = Prelude.Nothing,
      instancePoolsToUseCount = Prelude.Nothing,
      maintenanceStrategies = Prelude.Nothing,
      maxTotalPrice = Prelude.Nothing,
      minTargetCapacity = Prelude.Nothing,
      singleAvailabilityZone = Prelude.Nothing,
      singleInstanceType = Prelude.Nothing
    }

-- | The strategy that determines how to allocate the target Spot Instance
-- capacity across the Spot Instance pools specified by the EC2 Fleet
-- launch configuration. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-fleet-allocation-strategy.html Allocation strategies for Spot Instances>
-- in the /Amazon EC2 User Guide/.
--
-- [price-capacity-optimized (recommended)]
--     EC2 Fleet identifies the pools with the highest capacity
--     availability for the number of instances that are launching. This
--     means that we will request Spot Instances from the pools that we
--     believe have the lowest chance of interruption in the near term. EC2
--     Fleet then requests Spot Instances from the lowest priced of these
--     pools.
--
-- [capacity-optimized]
--     EC2 Fleet identifies the pools with the highest capacity
--     availability for the number of instances that are launching. This
--     means that we will request Spot Instances from the pools that we
--     believe have the lowest chance of interruption in the near term. To
--     give certain instance types a higher chance of launching first, use
--     @capacity-optimized-prioritized@. Set a priority for each instance
--     type by using the @Priority@ parameter for
--     @LaunchTemplateOverrides@. You can assign the same priority to
--     different @LaunchTemplateOverrides@. EC2 implements the priorities
--     on a best-effort basis, but optimizes for capacity first.
--     @capacity-optimized-prioritized@ is supported only if your EC2 Fleet
--     uses a launch template. Note that if the On-Demand
--     @AllocationStrategy@ is set to @prioritized@, the same priority is
--     applied when fulfilling On-Demand capacity.
--
-- [diversified]
--     EC2 Fleet requests instances from all of the Spot Instance pools
--     that you specify.
--
-- [lowest-price]
--     EC2 Fleet requests instances from the lowest priced Spot Instance
--     pool that has available capacity. If the lowest priced pool doesn\'t
--     have available capacity, the Spot Instances come from the next
--     lowest priced pool that has available capacity. If a pool runs out
--     of capacity before fulfilling your desired capacity, EC2 Fleet will
--     continue to fulfill your request by drawing from the next lowest
--     priced pool. To ensure that your desired capacity is met, you might
--     receive Spot Instances from several pools. Because this strategy
--     only considers instance price and not capacity availability, it
--     might lead to high interruption rates.
--
-- Default: @lowest-price@
spotOptionsRequest_allocationStrategy :: Lens.Lens' SpotOptionsRequest (Prelude.Maybe SpotAllocationStrategy)
spotOptionsRequest_allocationStrategy = Lens.lens (\SpotOptionsRequest' {allocationStrategy} -> allocationStrategy) (\s@SpotOptionsRequest' {} a -> s {allocationStrategy = a} :: SpotOptionsRequest)

-- | The behavior when a Spot Instance is interrupted.
--
-- Default: @terminate@
spotOptionsRequest_instanceInterruptionBehavior :: Lens.Lens' SpotOptionsRequest (Prelude.Maybe SpotInstanceInterruptionBehavior)
spotOptionsRequest_instanceInterruptionBehavior = Lens.lens (\SpotOptionsRequest' {instanceInterruptionBehavior} -> instanceInterruptionBehavior) (\s@SpotOptionsRequest' {} a -> s {instanceInterruptionBehavior = a} :: SpotOptionsRequest)

-- | The number of Spot pools across which to allocate your target Spot
-- capacity. Supported only when Spot @AllocationStrategy@ is set to
-- @lowest-price@. EC2 Fleet selects the cheapest Spot pools and evenly
-- allocates your target Spot capacity across the number of Spot pools that
-- you specify.
--
-- Note that EC2 Fleet attempts to draw Spot Instances from the number of
-- pools that you specify on a best effort basis. If a pool runs out of
-- Spot capacity before fulfilling your target capacity, EC2 Fleet will
-- continue to fulfill your request by drawing from the next cheapest pool.
-- To ensure that your target capacity is met, you might receive Spot
-- Instances from more than the number of pools that you specified.
-- Similarly, if most of the pools have no Spot capacity, you might receive
-- your full target capacity from fewer than the number of pools that you
-- specified.
spotOptionsRequest_instancePoolsToUseCount :: Lens.Lens' SpotOptionsRequest (Prelude.Maybe Prelude.Int)
spotOptionsRequest_instancePoolsToUseCount = Lens.lens (\SpotOptionsRequest' {instancePoolsToUseCount} -> instancePoolsToUseCount) (\s@SpotOptionsRequest' {} a -> s {instancePoolsToUseCount = a} :: SpotOptionsRequest)

-- | The strategies for managing your Spot Instances that are at an elevated
-- risk of being interrupted.
spotOptionsRequest_maintenanceStrategies :: Lens.Lens' SpotOptionsRequest (Prelude.Maybe FleetSpotMaintenanceStrategiesRequest)
spotOptionsRequest_maintenanceStrategies = Lens.lens (\SpotOptionsRequest' {maintenanceStrategies} -> maintenanceStrategies) (\s@SpotOptionsRequest' {} a -> s {maintenanceStrategies = a} :: SpotOptionsRequest)

-- | The maximum amount per hour for Spot Instances that you\'re willing to
-- pay. We do not recommend using this parameter because it can lead to
-- increased interruptions. If you do not specify this parameter, you will
-- pay the current Spot price.
--
-- If you specify a maximum price, your Spot Instances will be interrupted
-- more frequently than if you do not specify this parameter.
spotOptionsRequest_maxTotalPrice :: Lens.Lens' SpotOptionsRequest (Prelude.Maybe Prelude.Text)
spotOptionsRequest_maxTotalPrice = Lens.lens (\SpotOptionsRequest' {maxTotalPrice} -> maxTotalPrice) (\s@SpotOptionsRequest' {} a -> s {maxTotalPrice = a} :: SpotOptionsRequest)

-- | The minimum target capacity for Spot Instances in the fleet. If the
-- minimum target capacity is not reached, the fleet launches no instances.
--
-- Supported only for fleets of type @instant@.
--
-- At least one of the following must be specified:
-- @SingleAvailabilityZone@ | @SingleInstanceType@
spotOptionsRequest_minTargetCapacity :: Lens.Lens' SpotOptionsRequest (Prelude.Maybe Prelude.Int)
spotOptionsRequest_minTargetCapacity = Lens.lens (\SpotOptionsRequest' {minTargetCapacity} -> minTargetCapacity) (\s@SpotOptionsRequest' {} a -> s {minTargetCapacity = a} :: SpotOptionsRequest)

-- | Indicates that the fleet launches all Spot Instances into a single
-- Availability Zone.
--
-- Supported only for fleets of type @instant@.
spotOptionsRequest_singleAvailabilityZone :: Lens.Lens' SpotOptionsRequest (Prelude.Maybe Prelude.Bool)
spotOptionsRequest_singleAvailabilityZone = Lens.lens (\SpotOptionsRequest' {singleAvailabilityZone} -> singleAvailabilityZone) (\s@SpotOptionsRequest' {} a -> s {singleAvailabilityZone = a} :: SpotOptionsRequest)

-- | Indicates that the fleet uses a single instance type to launch all Spot
-- Instances in the fleet.
--
-- Supported only for fleets of type @instant@.
spotOptionsRequest_singleInstanceType :: Lens.Lens' SpotOptionsRequest (Prelude.Maybe Prelude.Bool)
spotOptionsRequest_singleInstanceType = Lens.lens (\SpotOptionsRequest' {singleInstanceType} -> singleInstanceType) (\s@SpotOptionsRequest' {} a -> s {singleInstanceType = a} :: SpotOptionsRequest)

instance Prelude.Hashable SpotOptionsRequest where
  hashWithSalt _salt SpotOptionsRequest' {..} =
    _salt `Prelude.hashWithSalt` allocationStrategy
      `Prelude.hashWithSalt` instanceInterruptionBehavior
      `Prelude.hashWithSalt` instancePoolsToUseCount
      `Prelude.hashWithSalt` maintenanceStrategies
      `Prelude.hashWithSalt` maxTotalPrice
      `Prelude.hashWithSalt` minTargetCapacity
      `Prelude.hashWithSalt` singleAvailabilityZone
      `Prelude.hashWithSalt` singleInstanceType

instance Prelude.NFData SpotOptionsRequest where
  rnf SpotOptionsRequest' {..} =
    Prelude.rnf allocationStrategy
      `Prelude.seq` Prelude.rnf instanceInterruptionBehavior
      `Prelude.seq` Prelude.rnf instancePoolsToUseCount
      `Prelude.seq` Prelude.rnf maintenanceStrategies
      `Prelude.seq` Prelude.rnf maxTotalPrice
      `Prelude.seq` Prelude.rnf minTargetCapacity
      `Prelude.seq` Prelude.rnf singleAvailabilityZone
      `Prelude.seq` Prelude.rnf singleInstanceType

instance Data.ToQuery SpotOptionsRequest where
  toQuery SpotOptionsRequest' {..} =
    Prelude.mconcat
      [ "AllocationStrategy" Data.=: allocationStrategy,
        "InstanceInterruptionBehavior"
          Data.=: instanceInterruptionBehavior,
        "InstancePoolsToUseCount"
          Data.=: instancePoolsToUseCount,
        "MaintenanceStrategies"
          Data.=: maintenanceStrategies,
        "MaxTotalPrice" Data.=: maxTotalPrice,
        "MinTargetCapacity" Data.=: minTargetCapacity,
        "SingleAvailabilityZone"
          Data.=: singleAvailabilityZone,
        "SingleInstanceType" Data.=: singleInstanceType
      ]
