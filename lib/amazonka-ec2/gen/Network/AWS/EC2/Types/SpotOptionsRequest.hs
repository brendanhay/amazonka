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
    sorInstanceInterruptionBehavior,
    sorSingleAvailabilityZone,
    sorMaxTotalPrice,
    sorMinTargetCapacity,
    sorInstancePoolsToUseCount,
    sorMaintenanceStrategies,
    sorSingleInstanceType,
    sorAllocationStrategy,
  )
where

import Network.AWS.EC2.Types.FleetSpotMaintenanceStrategiesRequest
import Network.AWS.EC2.Types.SpotAllocationStrategy
import Network.AWS.EC2.Types.SpotInstanceInterruptionBehavior
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the configuration of Spot Instances in an EC2 Fleet request.
--
-- /See:/ 'mkSpotOptionsRequest' smart constructor.
data SpotOptionsRequest = SpotOptionsRequest'
  { -- | The behavior when a Spot Instance is interrupted. The default is @terminate@ .
    instanceInterruptionBehavior :: Lude.Maybe SpotInstanceInterruptionBehavior,
    -- | Indicates that the fleet launches all Spot Instances into a single Availability Zone. Supported only for fleets of type @instant@ .
    singleAvailabilityZone :: Lude.Maybe Lude.Bool,
    -- | The maximum amount per hour for Spot Instances that you're willing to pay.
    maxTotalPrice :: Lude.Maybe Lude.Text,
    -- | The minimum target capacity for Spot Instances in the fleet. If the minimum target capacity is not reached, the fleet launches no instances.
    minTargetCapacity :: Lude.Maybe Lude.Int,
    -- | The number of Spot pools across which to allocate your target Spot capacity. Valid only when Spot __AllocationStrategy__ is set to @lowest-price@ . EC2 Fleet selects the cheapest Spot pools and evenly allocates your target Spot capacity across the number of Spot pools that you specify.
    instancePoolsToUseCount :: Lude.Maybe Lude.Int,
    -- | The strategies for managing your Spot Instances that are at an elevated risk of being interrupted.
    maintenanceStrategies :: Lude.Maybe FleetSpotMaintenanceStrategiesRequest,
    -- | Indicates that the fleet uses a single instance type to launch all Spot Instances in the fleet. Supported only for fleets of type @instant@ .
    singleInstanceType :: Lude.Maybe Lude.Bool,
    -- | Indicates how to allocate the target Spot Instance capacity across the Spot Instance pools specified by the EC2 Fleet.
    --
    -- If the allocation strategy is @lowest-price@ , EC2 Fleet launches instances from the Spot Instance pools with the lowest price. This is the default allocation strategy.
    -- If the allocation strategy is @diversified@ , EC2 Fleet launches instances from all of the Spot Instance pools that you specify.
    -- If the allocation strategy is @capacity-optimized@ , EC2 Fleet launches instances from Spot Instance pools with optimal capacity for the number of instances that are launching.
    allocationStrategy :: Lude.Maybe SpotAllocationStrategy
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SpotOptionsRequest' with the minimum fields required to make a request.
--
-- * 'instanceInterruptionBehavior' - The behavior when a Spot Instance is interrupted. The default is @terminate@ .
-- * 'singleAvailabilityZone' - Indicates that the fleet launches all Spot Instances into a single Availability Zone. Supported only for fleets of type @instant@ .
-- * 'maxTotalPrice' - The maximum amount per hour for Spot Instances that you're willing to pay.
-- * 'minTargetCapacity' - The minimum target capacity for Spot Instances in the fleet. If the minimum target capacity is not reached, the fleet launches no instances.
-- * 'instancePoolsToUseCount' - The number of Spot pools across which to allocate your target Spot capacity. Valid only when Spot __AllocationStrategy__ is set to @lowest-price@ . EC2 Fleet selects the cheapest Spot pools and evenly allocates your target Spot capacity across the number of Spot pools that you specify.
-- * 'maintenanceStrategies' - The strategies for managing your Spot Instances that are at an elevated risk of being interrupted.
-- * 'singleInstanceType' - Indicates that the fleet uses a single instance type to launch all Spot Instances in the fleet. Supported only for fleets of type @instant@ .
-- * 'allocationStrategy' - Indicates how to allocate the target Spot Instance capacity across the Spot Instance pools specified by the EC2 Fleet.
--
-- If the allocation strategy is @lowest-price@ , EC2 Fleet launches instances from the Spot Instance pools with the lowest price. This is the default allocation strategy.
-- If the allocation strategy is @diversified@ , EC2 Fleet launches instances from all of the Spot Instance pools that you specify.
-- If the allocation strategy is @capacity-optimized@ , EC2 Fleet launches instances from Spot Instance pools with optimal capacity for the number of instances that are launching.
mkSpotOptionsRequest ::
  SpotOptionsRequest
mkSpotOptionsRequest =
  SpotOptionsRequest'
    { instanceInterruptionBehavior = Lude.Nothing,
      singleAvailabilityZone = Lude.Nothing,
      maxTotalPrice = Lude.Nothing,
      minTargetCapacity = Lude.Nothing,
      instancePoolsToUseCount = Lude.Nothing,
      maintenanceStrategies = Lude.Nothing,
      singleInstanceType = Lude.Nothing,
      allocationStrategy = Lude.Nothing
    }

-- | The behavior when a Spot Instance is interrupted. The default is @terminate@ .
--
-- /Note:/ Consider using 'instanceInterruptionBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sorInstanceInterruptionBehavior :: Lens.Lens' SpotOptionsRequest (Lude.Maybe SpotInstanceInterruptionBehavior)
sorInstanceInterruptionBehavior = Lens.lens (instanceInterruptionBehavior :: SpotOptionsRequest -> Lude.Maybe SpotInstanceInterruptionBehavior) (\s a -> s {instanceInterruptionBehavior = a} :: SpotOptionsRequest)
{-# DEPRECATED sorInstanceInterruptionBehavior "Use generic-lens or generic-optics with 'instanceInterruptionBehavior' instead." #-}

-- | Indicates that the fleet launches all Spot Instances into a single Availability Zone. Supported only for fleets of type @instant@ .
--
-- /Note:/ Consider using 'singleAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sorSingleAvailabilityZone :: Lens.Lens' SpotOptionsRequest (Lude.Maybe Lude.Bool)
sorSingleAvailabilityZone = Lens.lens (singleAvailabilityZone :: SpotOptionsRequest -> Lude.Maybe Lude.Bool) (\s a -> s {singleAvailabilityZone = a} :: SpotOptionsRequest)
{-# DEPRECATED sorSingleAvailabilityZone "Use generic-lens or generic-optics with 'singleAvailabilityZone' instead." #-}

-- | The maximum amount per hour for Spot Instances that you're willing to pay.
--
-- /Note:/ Consider using 'maxTotalPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sorMaxTotalPrice :: Lens.Lens' SpotOptionsRequest (Lude.Maybe Lude.Text)
sorMaxTotalPrice = Lens.lens (maxTotalPrice :: SpotOptionsRequest -> Lude.Maybe Lude.Text) (\s a -> s {maxTotalPrice = a} :: SpotOptionsRequest)
{-# DEPRECATED sorMaxTotalPrice "Use generic-lens or generic-optics with 'maxTotalPrice' instead." #-}

-- | The minimum target capacity for Spot Instances in the fleet. If the minimum target capacity is not reached, the fleet launches no instances.
--
-- /Note:/ Consider using 'minTargetCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sorMinTargetCapacity :: Lens.Lens' SpotOptionsRequest (Lude.Maybe Lude.Int)
sorMinTargetCapacity = Lens.lens (minTargetCapacity :: SpotOptionsRequest -> Lude.Maybe Lude.Int) (\s a -> s {minTargetCapacity = a} :: SpotOptionsRequest)
{-# DEPRECATED sorMinTargetCapacity "Use generic-lens or generic-optics with 'minTargetCapacity' instead." #-}

-- | The number of Spot pools across which to allocate your target Spot capacity. Valid only when Spot __AllocationStrategy__ is set to @lowest-price@ . EC2 Fleet selects the cheapest Spot pools and evenly allocates your target Spot capacity across the number of Spot pools that you specify.
--
-- /Note:/ Consider using 'instancePoolsToUseCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sorInstancePoolsToUseCount :: Lens.Lens' SpotOptionsRequest (Lude.Maybe Lude.Int)
sorInstancePoolsToUseCount = Lens.lens (instancePoolsToUseCount :: SpotOptionsRequest -> Lude.Maybe Lude.Int) (\s a -> s {instancePoolsToUseCount = a} :: SpotOptionsRequest)
{-# DEPRECATED sorInstancePoolsToUseCount "Use generic-lens or generic-optics with 'instancePoolsToUseCount' instead." #-}

-- | The strategies for managing your Spot Instances that are at an elevated risk of being interrupted.
--
-- /Note:/ Consider using 'maintenanceStrategies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sorMaintenanceStrategies :: Lens.Lens' SpotOptionsRequest (Lude.Maybe FleetSpotMaintenanceStrategiesRequest)
sorMaintenanceStrategies = Lens.lens (maintenanceStrategies :: SpotOptionsRequest -> Lude.Maybe FleetSpotMaintenanceStrategiesRequest) (\s a -> s {maintenanceStrategies = a} :: SpotOptionsRequest)
{-# DEPRECATED sorMaintenanceStrategies "Use generic-lens or generic-optics with 'maintenanceStrategies' instead." #-}

-- | Indicates that the fleet uses a single instance type to launch all Spot Instances in the fleet. Supported only for fleets of type @instant@ .
--
-- /Note:/ Consider using 'singleInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sorSingleInstanceType :: Lens.Lens' SpotOptionsRequest (Lude.Maybe Lude.Bool)
sorSingleInstanceType = Lens.lens (singleInstanceType :: SpotOptionsRequest -> Lude.Maybe Lude.Bool) (\s a -> s {singleInstanceType = a} :: SpotOptionsRequest)
{-# DEPRECATED sorSingleInstanceType "Use generic-lens or generic-optics with 'singleInstanceType' instead." #-}

-- | Indicates how to allocate the target Spot Instance capacity across the Spot Instance pools specified by the EC2 Fleet.
--
-- If the allocation strategy is @lowest-price@ , EC2 Fleet launches instances from the Spot Instance pools with the lowest price. This is the default allocation strategy.
-- If the allocation strategy is @diversified@ , EC2 Fleet launches instances from all of the Spot Instance pools that you specify.
-- If the allocation strategy is @capacity-optimized@ , EC2 Fleet launches instances from Spot Instance pools with optimal capacity for the number of instances that are launching.
--
-- /Note:/ Consider using 'allocationStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sorAllocationStrategy :: Lens.Lens' SpotOptionsRequest (Lude.Maybe SpotAllocationStrategy)
sorAllocationStrategy = Lens.lens (allocationStrategy :: SpotOptionsRequest -> Lude.Maybe SpotAllocationStrategy) (\s a -> s {allocationStrategy = a} :: SpotOptionsRequest)
{-# DEPRECATED sorAllocationStrategy "Use generic-lens or generic-optics with 'allocationStrategy' instead." #-}

instance Lude.ToQuery SpotOptionsRequest where
  toQuery SpotOptionsRequest' {..} =
    Lude.mconcat
      [ "InstanceInterruptionBehavior"
          Lude.=: instanceInterruptionBehavior,
        "SingleAvailabilityZone" Lude.=: singleAvailabilityZone,
        "MaxTotalPrice" Lude.=: maxTotalPrice,
        "MinTargetCapacity" Lude.=: minTargetCapacity,
        "InstancePoolsToUseCount" Lude.=: instancePoolsToUseCount,
        "MaintenanceStrategies" Lude.=: maintenanceStrategies,
        "SingleInstanceType" Lude.=: singleInstanceType,
        "AllocationStrategy" Lude.=: allocationStrategy
      ]
