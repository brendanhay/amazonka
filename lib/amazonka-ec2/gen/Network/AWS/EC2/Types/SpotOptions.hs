{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotOptions
  ( SpotOptions (..),

    -- * Smart constructor
    mkSpotOptions,

    -- * Lenses
    soAllocationStrategy,
    soInstanceInterruptionBehavior,
    soInstancePoolsToUseCount,
    soMaintenanceStrategies,
    soMaxTotalPrice,
    soMinTargetCapacity,
    soSingleAvailabilityZone,
    soSingleInstanceType,
  )
where

import qualified Network.AWS.EC2.Types.FleetSpotMaintenanceStrategies as Types
import qualified Network.AWS.EC2.Types.SpotAllocationStrategy as Types
import qualified Network.AWS.EC2.Types.SpotInstanceInterruptionBehavior as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the configuration of Spot Instances in an EC2 Fleet.
--
-- /See:/ 'mkSpotOptions' smart constructor.
data SpotOptions = SpotOptions'
  { -- | Indicates how to allocate the target Spot Instance capacity across the Spot Instance pools specified by the EC2 Fleet.
    --
    -- If the allocation strategy is @lowest-price@ , EC2 Fleet launches instances from the Spot Instance pools with the lowest price. This is the default allocation strategy.
    -- If the allocation strategy is @diversified@ , EC2 Fleet launches instances from all of the Spot Instance pools that you specify.
    -- If the allocation strategy is @capacity-optimized@ , EC2 Fleet launches instances from Spot Instance pools with optimal capacity for the number of instances that are launching.
    allocationStrategy :: Core.Maybe Types.SpotAllocationStrategy,
    -- | The behavior when a Spot Instance is interrupted. The default is @terminate@ .
    instanceInterruptionBehavior :: Core.Maybe Types.SpotInstanceInterruptionBehavior,
    -- | The number of Spot pools across which to allocate your target Spot capacity. Valid only when __AllocationStrategy__ is set to @lowest-price@ . EC2 Fleet selects the cheapest Spot pools and evenly allocates your target Spot capacity across the number of Spot pools that you specify.
    instancePoolsToUseCount :: Core.Maybe Core.Int,
    -- | The strategies for managing your workloads on your Spot Instances that will be interrupted. Currently only the capacity rebalance strategy is available.
    maintenanceStrategies :: Core.Maybe Types.FleetSpotMaintenanceStrategies,
    -- | The maximum amount per hour for Spot Instances that you're willing to pay.
    maxTotalPrice :: Core.Maybe Types.String,
    -- | The minimum target capacity for Spot Instances in the fleet. If the minimum target capacity is not reached, the fleet launches no instances.
    minTargetCapacity :: Core.Maybe Core.Int,
    -- | Indicates that the fleet launches all Spot Instances into a single Availability Zone. Supported only for fleets of type @instant@ .
    singleAvailabilityZone :: Core.Maybe Core.Bool,
    -- | Indicates that the fleet uses a single instance type to launch all Spot Instances in the fleet. Supported only for fleets of type @instant@ .
    singleInstanceType :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SpotOptions' value with any optional fields omitted.
mkSpotOptions ::
  SpotOptions
mkSpotOptions =
  SpotOptions'
    { allocationStrategy = Core.Nothing,
      instanceInterruptionBehavior = Core.Nothing,
      instancePoolsToUseCount = Core.Nothing,
      maintenanceStrategies = Core.Nothing,
      maxTotalPrice = Core.Nothing,
      minTargetCapacity = Core.Nothing,
      singleAvailabilityZone = Core.Nothing,
      singleInstanceType = Core.Nothing
    }

-- | Indicates how to allocate the target Spot Instance capacity across the Spot Instance pools specified by the EC2 Fleet.
--
-- If the allocation strategy is @lowest-price@ , EC2 Fleet launches instances from the Spot Instance pools with the lowest price. This is the default allocation strategy.
-- If the allocation strategy is @diversified@ , EC2 Fleet launches instances from all of the Spot Instance pools that you specify.
-- If the allocation strategy is @capacity-optimized@ , EC2 Fleet launches instances from Spot Instance pools with optimal capacity for the number of instances that are launching.
--
-- /Note:/ Consider using 'allocationStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
soAllocationStrategy :: Lens.Lens' SpotOptions (Core.Maybe Types.SpotAllocationStrategy)
soAllocationStrategy = Lens.field @"allocationStrategy"
{-# DEPRECATED soAllocationStrategy "Use generic-lens or generic-optics with 'allocationStrategy' instead." #-}

-- | The behavior when a Spot Instance is interrupted. The default is @terminate@ .
--
-- /Note:/ Consider using 'instanceInterruptionBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
soInstanceInterruptionBehavior :: Lens.Lens' SpotOptions (Core.Maybe Types.SpotInstanceInterruptionBehavior)
soInstanceInterruptionBehavior = Lens.field @"instanceInterruptionBehavior"
{-# DEPRECATED soInstanceInterruptionBehavior "Use generic-lens or generic-optics with 'instanceInterruptionBehavior' instead." #-}

-- | The number of Spot pools across which to allocate your target Spot capacity. Valid only when __AllocationStrategy__ is set to @lowest-price@ . EC2 Fleet selects the cheapest Spot pools and evenly allocates your target Spot capacity across the number of Spot pools that you specify.
--
-- /Note:/ Consider using 'instancePoolsToUseCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
soInstancePoolsToUseCount :: Lens.Lens' SpotOptions (Core.Maybe Core.Int)
soInstancePoolsToUseCount = Lens.field @"instancePoolsToUseCount"
{-# DEPRECATED soInstancePoolsToUseCount "Use generic-lens or generic-optics with 'instancePoolsToUseCount' instead." #-}

-- | The strategies for managing your workloads on your Spot Instances that will be interrupted. Currently only the capacity rebalance strategy is available.
--
-- /Note:/ Consider using 'maintenanceStrategies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
soMaintenanceStrategies :: Lens.Lens' SpotOptions (Core.Maybe Types.FleetSpotMaintenanceStrategies)
soMaintenanceStrategies = Lens.field @"maintenanceStrategies"
{-# DEPRECATED soMaintenanceStrategies "Use generic-lens or generic-optics with 'maintenanceStrategies' instead." #-}

-- | The maximum amount per hour for Spot Instances that you're willing to pay.
--
-- /Note:/ Consider using 'maxTotalPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
soMaxTotalPrice :: Lens.Lens' SpotOptions (Core.Maybe Types.String)
soMaxTotalPrice = Lens.field @"maxTotalPrice"
{-# DEPRECATED soMaxTotalPrice "Use generic-lens or generic-optics with 'maxTotalPrice' instead." #-}

-- | The minimum target capacity for Spot Instances in the fleet. If the minimum target capacity is not reached, the fleet launches no instances.
--
-- /Note:/ Consider using 'minTargetCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
soMinTargetCapacity :: Lens.Lens' SpotOptions (Core.Maybe Core.Int)
soMinTargetCapacity = Lens.field @"minTargetCapacity"
{-# DEPRECATED soMinTargetCapacity "Use generic-lens or generic-optics with 'minTargetCapacity' instead." #-}

-- | Indicates that the fleet launches all Spot Instances into a single Availability Zone. Supported only for fleets of type @instant@ .
--
-- /Note:/ Consider using 'singleAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
soSingleAvailabilityZone :: Lens.Lens' SpotOptions (Core.Maybe Core.Bool)
soSingleAvailabilityZone = Lens.field @"singleAvailabilityZone"
{-# DEPRECATED soSingleAvailabilityZone "Use generic-lens or generic-optics with 'singleAvailabilityZone' instead." #-}

-- | Indicates that the fleet uses a single instance type to launch all Spot Instances in the fleet. Supported only for fleets of type @instant@ .
--
-- /Note:/ Consider using 'singleInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
soSingleInstanceType :: Lens.Lens' SpotOptions (Core.Maybe Core.Bool)
soSingleInstanceType = Lens.field @"singleInstanceType"
{-# DEPRECATED soSingleInstanceType "Use generic-lens or generic-optics with 'singleInstanceType' instead." #-}

instance Core.FromXML SpotOptions where
  parseXML x =
    SpotOptions'
      Core.<$> (x Core..@? "allocationStrategy")
      Core.<*> (x Core..@? "instanceInterruptionBehavior")
      Core.<*> (x Core..@? "instancePoolsToUseCount")
      Core.<*> (x Core..@? "maintenanceStrategies")
      Core.<*> (x Core..@? "maxTotalPrice")
      Core.<*> (x Core..@? "minTargetCapacity")
      Core.<*> (x Core..@? "singleAvailabilityZone")
      Core.<*> (x Core..@? "singleInstanceType")
