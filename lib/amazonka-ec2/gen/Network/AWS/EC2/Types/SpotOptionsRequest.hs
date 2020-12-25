{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.SpotOptionsRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.SpotOptionsRequest
  ( SpotOptionsRequest (..),

    -- * Smart constructor
    mkSpotOptionsRequest,

    -- * Lenses
    sorAllocationStrategy,
    sorInstanceInterruptionBehavior,
    sorInstancePoolsToUseCount,
    sorMaintenanceStrategies,
    sorMaxTotalPrice,
    sorMinTargetCapacity,
    sorSingleAvailabilityZone,
    sorSingleInstanceType,
  )
where

import qualified Network.AWS.EC2.Types.FleetSpotMaintenanceStrategiesRequest as Types
import qualified Network.AWS.EC2.Types.SpotAllocationStrategy as Types
import qualified Network.AWS.EC2.Types.SpotInstanceInterruptionBehavior as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the configuration of Spot Instances in an EC2 Fleet request.
--
-- /See:/ 'mkSpotOptionsRequest' smart constructor.
data SpotOptionsRequest = SpotOptionsRequest'
  { -- | Indicates how to allocate the target Spot Instance capacity across the Spot Instance pools specified by the EC2 Fleet.
    --
    -- If the allocation strategy is @lowest-price@ , EC2 Fleet launches instances from the Spot Instance pools with the lowest price. This is the default allocation strategy.
    -- If the allocation strategy is @diversified@ , EC2 Fleet launches instances from all of the Spot Instance pools that you specify.
    -- If the allocation strategy is @capacity-optimized@ , EC2 Fleet launches instances from Spot Instance pools with optimal capacity for the number of instances that are launching.
    allocationStrategy :: Core.Maybe Types.SpotAllocationStrategy,
    -- | The behavior when a Spot Instance is interrupted. The default is @terminate@ .
    instanceInterruptionBehavior :: Core.Maybe Types.SpotInstanceInterruptionBehavior,
    -- | The number of Spot pools across which to allocate your target Spot capacity. Valid only when Spot __AllocationStrategy__ is set to @lowest-price@ . EC2 Fleet selects the cheapest Spot pools and evenly allocates your target Spot capacity across the number of Spot pools that you specify.
    instancePoolsToUseCount :: Core.Maybe Core.Int,
    -- | The strategies for managing your Spot Instances that are at an elevated risk of being interrupted.
    maintenanceStrategies :: Core.Maybe Types.FleetSpotMaintenanceStrategiesRequest,
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

-- | Creates a 'SpotOptionsRequest' value with any optional fields omitted.
mkSpotOptionsRequest ::
  SpotOptionsRequest
mkSpotOptionsRequest =
  SpotOptionsRequest'
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
sorAllocationStrategy :: Lens.Lens' SpotOptionsRequest (Core.Maybe Types.SpotAllocationStrategy)
sorAllocationStrategy = Lens.field @"allocationStrategy"
{-# DEPRECATED sorAllocationStrategy "Use generic-lens or generic-optics with 'allocationStrategy' instead." #-}

-- | The behavior when a Spot Instance is interrupted. The default is @terminate@ .
--
-- /Note:/ Consider using 'instanceInterruptionBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sorInstanceInterruptionBehavior :: Lens.Lens' SpotOptionsRequest (Core.Maybe Types.SpotInstanceInterruptionBehavior)
sorInstanceInterruptionBehavior = Lens.field @"instanceInterruptionBehavior"
{-# DEPRECATED sorInstanceInterruptionBehavior "Use generic-lens or generic-optics with 'instanceInterruptionBehavior' instead." #-}

-- | The number of Spot pools across which to allocate your target Spot capacity. Valid only when Spot __AllocationStrategy__ is set to @lowest-price@ . EC2 Fleet selects the cheapest Spot pools and evenly allocates your target Spot capacity across the number of Spot pools that you specify.
--
-- /Note:/ Consider using 'instancePoolsToUseCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sorInstancePoolsToUseCount :: Lens.Lens' SpotOptionsRequest (Core.Maybe Core.Int)
sorInstancePoolsToUseCount = Lens.field @"instancePoolsToUseCount"
{-# DEPRECATED sorInstancePoolsToUseCount "Use generic-lens or generic-optics with 'instancePoolsToUseCount' instead." #-}

-- | The strategies for managing your Spot Instances that are at an elevated risk of being interrupted.
--
-- /Note:/ Consider using 'maintenanceStrategies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sorMaintenanceStrategies :: Lens.Lens' SpotOptionsRequest (Core.Maybe Types.FleetSpotMaintenanceStrategiesRequest)
sorMaintenanceStrategies = Lens.field @"maintenanceStrategies"
{-# DEPRECATED sorMaintenanceStrategies "Use generic-lens or generic-optics with 'maintenanceStrategies' instead." #-}

-- | The maximum amount per hour for Spot Instances that you're willing to pay.
--
-- /Note:/ Consider using 'maxTotalPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sorMaxTotalPrice :: Lens.Lens' SpotOptionsRequest (Core.Maybe Types.String)
sorMaxTotalPrice = Lens.field @"maxTotalPrice"
{-# DEPRECATED sorMaxTotalPrice "Use generic-lens or generic-optics with 'maxTotalPrice' instead." #-}

-- | The minimum target capacity for Spot Instances in the fleet. If the minimum target capacity is not reached, the fleet launches no instances.
--
-- /Note:/ Consider using 'minTargetCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sorMinTargetCapacity :: Lens.Lens' SpotOptionsRequest (Core.Maybe Core.Int)
sorMinTargetCapacity = Lens.field @"minTargetCapacity"
{-# DEPRECATED sorMinTargetCapacity "Use generic-lens or generic-optics with 'minTargetCapacity' instead." #-}

-- | Indicates that the fleet launches all Spot Instances into a single Availability Zone. Supported only for fleets of type @instant@ .
--
-- /Note:/ Consider using 'singleAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sorSingleAvailabilityZone :: Lens.Lens' SpotOptionsRequest (Core.Maybe Core.Bool)
sorSingleAvailabilityZone = Lens.field @"singleAvailabilityZone"
{-# DEPRECATED sorSingleAvailabilityZone "Use generic-lens or generic-optics with 'singleAvailabilityZone' instead." #-}

-- | Indicates that the fleet uses a single instance type to launch all Spot Instances in the fleet. Supported only for fleets of type @instant@ .
--
-- /Note:/ Consider using 'singleInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sorSingleInstanceType :: Lens.Lens' SpotOptionsRequest (Core.Maybe Core.Bool)
sorSingleInstanceType = Lens.field @"singleInstanceType"
{-# DEPRECATED sorSingleInstanceType "Use generic-lens or generic-optics with 'singleInstanceType' instead." #-}
