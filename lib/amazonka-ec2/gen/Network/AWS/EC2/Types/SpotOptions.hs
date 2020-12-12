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
    soInstanceInterruptionBehavior,
    soSingleAvailabilityZone,
    soMaxTotalPrice,
    soMinTargetCapacity,
    soInstancePoolsToUseCount,
    soMaintenanceStrategies,
    soSingleInstanceType,
    soAllocationStrategy,
  )
where

import Network.AWS.EC2.Types.FleetSpotMaintenanceStrategies
import Network.AWS.EC2.Types.SpotAllocationStrategy
import Network.AWS.EC2.Types.SpotInstanceInterruptionBehavior
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the configuration of Spot Instances in an EC2 Fleet.
--
-- /See:/ 'mkSpotOptions' smart constructor.
data SpotOptions = SpotOptions'
  { instanceInterruptionBehavior ::
      Lude.Maybe SpotInstanceInterruptionBehavior,
    singleAvailabilityZone :: Lude.Maybe Lude.Bool,
    maxTotalPrice :: Lude.Maybe Lude.Text,
    minTargetCapacity :: Lude.Maybe Lude.Int,
    instancePoolsToUseCount :: Lude.Maybe Lude.Int,
    maintenanceStrategies :: Lude.Maybe FleetSpotMaintenanceStrategies,
    singleInstanceType :: Lude.Maybe Lude.Bool,
    allocationStrategy :: Lude.Maybe SpotAllocationStrategy
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SpotOptions' with the minimum fields required to make a request.
--
-- * 'allocationStrategy' - Indicates how to allocate the target Spot Instance capacity across the Spot Instance pools specified by the EC2 Fleet.
--
-- If the allocation strategy is @lowest-price@ , EC2 Fleet launches instances from the Spot Instance pools with the lowest price. This is the default allocation strategy.
-- If the allocation strategy is @diversified@ , EC2 Fleet launches instances from all of the Spot Instance pools that you specify.
-- If the allocation strategy is @capacity-optimized@ , EC2 Fleet launches instances from Spot Instance pools with optimal capacity for the number of instances that are launching.
-- * 'instanceInterruptionBehavior' - The behavior when a Spot Instance is interrupted. The default is @terminate@ .
-- * 'instancePoolsToUseCount' - The number of Spot pools across which to allocate your target Spot capacity. Valid only when __AllocationStrategy__ is set to @lowest-price@ . EC2 Fleet selects the cheapest Spot pools and evenly allocates your target Spot capacity across the number of Spot pools that you specify.
-- * 'maintenanceStrategies' - The strategies for managing your workloads on your Spot Instances that will be interrupted. Currently only the capacity rebalance strategy is available.
-- * 'maxTotalPrice' - The maximum amount per hour for Spot Instances that you're willing to pay.
-- * 'minTargetCapacity' - The minimum target capacity for Spot Instances in the fleet. If the minimum target capacity is not reached, the fleet launches no instances.
-- * 'singleAvailabilityZone' - Indicates that the fleet launches all Spot Instances into a single Availability Zone. Supported only for fleets of type @instant@ .
-- * 'singleInstanceType' - Indicates that the fleet uses a single instance type to launch all Spot Instances in the fleet. Supported only for fleets of type @instant@ .
mkSpotOptions ::
  SpotOptions
mkSpotOptions =
  SpotOptions'
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
soInstanceInterruptionBehavior :: Lens.Lens' SpotOptions (Lude.Maybe SpotInstanceInterruptionBehavior)
soInstanceInterruptionBehavior = Lens.lens (instanceInterruptionBehavior :: SpotOptions -> Lude.Maybe SpotInstanceInterruptionBehavior) (\s a -> s {instanceInterruptionBehavior = a} :: SpotOptions)
{-# DEPRECATED soInstanceInterruptionBehavior "Use generic-lens or generic-optics with 'instanceInterruptionBehavior' instead." #-}

-- | Indicates that the fleet launches all Spot Instances into a single Availability Zone. Supported only for fleets of type @instant@ .
--
-- /Note:/ Consider using 'singleAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
soSingleAvailabilityZone :: Lens.Lens' SpotOptions (Lude.Maybe Lude.Bool)
soSingleAvailabilityZone = Lens.lens (singleAvailabilityZone :: SpotOptions -> Lude.Maybe Lude.Bool) (\s a -> s {singleAvailabilityZone = a} :: SpotOptions)
{-# DEPRECATED soSingleAvailabilityZone "Use generic-lens or generic-optics with 'singleAvailabilityZone' instead." #-}

-- | The maximum amount per hour for Spot Instances that you're willing to pay.
--
-- /Note:/ Consider using 'maxTotalPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
soMaxTotalPrice :: Lens.Lens' SpotOptions (Lude.Maybe Lude.Text)
soMaxTotalPrice = Lens.lens (maxTotalPrice :: SpotOptions -> Lude.Maybe Lude.Text) (\s a -> s {maxTotalPrice = a} :: SpotOptions)
{-# DEPRECATED soMaxTotalPrice "Use generic-lens or generic-optics with 'maxTotalPrice' instead." #-}

-- | The minimum target capacity for Spot Instances in the fleet. If the minimum target capacity is not reached, the fleet launches no instances.
--
-- /Note:/ Consider using 'minTargetCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
soMinTargetCapacity :: Lens.Lens' SpotOptions (Lude.Maybe Lude.Int)
soMinTargetCapacity = Lens.lens (minTargetCapacity :: SpotOptions -> Lude.Maybe Lude.Int) (\s a -> s {minTargetCapacity = a} :: SpotOptions)
{-# DEPRECATED soMinTargetCapacity "Use generic-lens or generic-optics with 'minTargetCapacity' instead." #-}

-- | The number of Spot pools across which to allocate your target Spot capacity. Valid only when __AllocationStrategy__ is set to @lowest-price@ . EC2 Fleet selects the cheapest Spot pools and evenly allocates your target Spot capacity across the number of Spot pools that you specify.
--
-- /Note:/ Consider using 'instancePoolsToUseCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
soInstancePoolsToUseCount :: Lens.Lens' SpotOptions (Lude.Maybe Lude.Int)
soInstancePoolsToUseCount = Lens.lens (instancePoolsToUseCount :: SpotOptions -> Lude.Maybe Lude.Int) (\s a -> s {instancePoolsToUseCount = a} :: SpotOptions)
{-# DEPRECATED soInstancePoolsToUseCount "Use generic-lens or generic-optics with 'instancePoolsToUseCount' instead." #-}

-- | The strategies for managing your workloads on your Spot Instances that will be interrupted. Currently only the capacity rebalance strategy is available.
--
-- /Note:/ Consider using 'maintenanceStrategies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
soMaintenanceStrategies :: Lens.Lens' SpotOptions (Lude.Maybe FleetSpotMaintenanceStrategies)
soMaintenanceStrategies = Lens.lens (maintenanceStrategies :: SpotOptions -> Lude.Maybe FleetSpotMaintenanceStrategies) (\s a -> s {maintenanceStrategies = a} :: SpotOptions)
{-# DEPRECATED soMaintenanceStrategies "Use generic-lens or generic-optics with 'maintenanceStrategies' instead." #-}

-- | Indicates that the fleet uses a single instance type to launch all Spot Instances in the fleet. Supported only for fleets of type @instant@ .
--
-- /Note:/ Consider using 'singleInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
soSingleInstanceType :: Lens.Lens' SpotOptions (Lude.Maybe Lude.Bool)
soSingleInstanceType = Lens.lens (singleInstanceType :: SpotOptions -> Lude.Maybe Lude.Bool) (\s a -> s {singleInstanceType = a} :: SpotOptions)
{-# DEPRECATED soSingleInstanceType "Use generic-lens or generic-optics with 'singleInstanceType' instead." #-}

-- | Indicates how to allocate the target Spot Instance capacity across the Spot Instance pools specified by the EC2 Fleet.
--
-- If the allocation strategy is @lowest-price@ , EC2 Fleet launches instances from the Spot Instance pools with the lowest price. This is the default allocation strategy.
-- If the allocation strategy is @diversified@ , EC2 Fleet launches instances from all of the Spot Instance pools that you specify.
-- If the allocation strategy is @capacity-optimized@ , EC2 Fleet launches instances from Spot Instance pools with optimal capacity for the number of instances that are launching.
--
-- /Note:/ Consider using 'allocationStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
soAllocationStrategy :: Lens.Lens' SpotOptions (Lude.Maybe SpotAllocationStrategy)
soAllocationStrategy = Lens.lens (allocationStrategy :: SpotOptions -> Lude.Maybe SpotAllocationStrategy) (\s a -> s {allocationStrategy = a} :: SpotOptions)
{-# DEPRECATED soAllocationStrategy "Use generic-lens or generic-optics with 'allocationStrategy' instead." #-}

instance Lude.FromXML SpotOptions where
  parseXML x =
    SpotOptions'
      Lude.<$> (x Lude..@? "instanceInterruptionBehavior")
      Lude.<*> (x Lude..@? "singleAvailabilityZone")
      Lude.<*> (x Lude..@? "maxTotalPrice")
      Lude.<*> (x Lude..@? "minTargetCapacity")
      Lude.<*> (x Lude..@? "instancePoolsToUseCount")
      Lude.<*> (x Lude..@? "maintenanceStrategies")
      Lude.<*> (x Lude..@? "singleInstanceType")
      Lude.<*> (x Lude..@? "allocationStrategy")
