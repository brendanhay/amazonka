{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.OnDemandOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.OnDemandOptions
  ( OnDemandOptions (..),

    -- * Smart constructor
    mkOnDemandOptions,

    -- * Lenses
    odoAllocationStrategy,
    odoCapacityReservationOptions,
    odoMaxTotalPrice,
    odoMinTargetCapacity,
    odoSingleAvailabilityZone,
    odoSingleInstanceType,
  )
where

import qualified Network.AWS.EC2.Types.CapacityReservationOptions as Types
import qualified Network.AWS.EC2.Types.FleetOnDemandAllocationStrategy as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the configuration of On-Demand Instances in an EC2 Fleet.
--
-- /See:/ 'mkOnDemandOptions' smart constructor.
data OnDemandOptions = OnDemandOptions'
  { -- | The order of the launch template overrides to use in fulfilling On-Demand capacity. If you specify @lowest-price@ , EC2 Fleet uses price to determine the order, launching the lowest price first. If you specify @prioritized@ , EC2 Fleet uses the priority that you assigned to each launch template override, launching the highest priority first. If you do not specify a value, EC2 Fleet defaults to @lowest-price@ .
    allocationStrategy :: Core.Maybe Types.FleetOnDemandAllocationStrategy,
    -- | The strategy for using unused Capacity Reservations for fulfilling On-Demand capacity. Supported only for fleets of type @instant@ .
    capacityReservationOptions :: Core.Maybe Types.CapacityReservationOptions,
    -- | The maximum amount per hour for On-Demand Instances that you're willing to pay.
    maxTotalPrice :: Core.Maybe Types.String,
    -- | The minimum target capacity for On-Demand Instances in the fleet. If the minimum target capacity is not reached, the fleet launches no instances.
    minTargetCapacity :: Core.Maybe Core.Int,
    -- | Indicates that the fleet launches all On-Demand Instances into a single Availability Zone. Supported only for fleets of type @instant@ .
    singleAvailabilityZone :: Core.Maybe Core.Bool,
    -- | Indicates that the fleet uses a single instance type to launch all On-Demand Instances in the fleet. Supported only for fleets of type @instant@ .
    singleInstanceType :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OnDemandOptions' value with any optional fields omitted.
mkOnDemandOptions ::
  OnDemandOptions
mkOnDemandOptions =
  OnDemandOptions'
    { allocationStrategy = Core.Nothing,
      capacityReservationOptions = Core.Nothing,
      maxTotalPrice = Core.Nothing,
      minTargetCapacity = Core.Nothing,
      singleAvailabilityZone = Core.Nothing,
      singleInstanceType = Core.Nothing
    }

-- | The order of the launch template overrides to use in fulfilling On-Demand capacity. If you specify @lowest-price@ , EC2 Fleet uses price to determine the order, launching the lowest price first. If you specify @prioritized@ , EC2 Fleet uses the priority that you assigned to each launch template override, launching the highest priority first. If you do not specify a value, EC2 Fleet defaults to @lowest-price@ .
--
-- /Note:/ Consider using 'allocationStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odoAllocationStrategy :: Lens.Lens' OnDemandOptions (Core.Maybe Types.FleetOnDemandAllocationStrategy)
odoAllocationStrategy = Lens.field @"allocationStrategy"
{-# DEPRECATED odoAllocationStrategy "Use generic-lens or generic-optics with 'allocationStrategy' instead." #-}

-- | The strategy for using unused Capacity Reservations for fulfilling On-Demand capacity. Supported only for fleets of type @instant@ .
--
-- /Note:/ Consider using 'capacityReservationOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odoCapacityReservationOptions :: Lens.Lens' OnDemandOptions (Core.Maybe Types.CapacityReservationOptions)
odoCapacityReservationOptions = Lens.field @"capacityReservationOptions"
{-# DEPRECATED odoCapacityReservationOptions "Use generic-lens or generic-optics with 'capacityReservationOptions' instead." #-}

-- | The maximum amount per hour for On-Demand Instances that you're willing to pay.
--
-- /Note:/ Consider using 'maxTotalPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odoMaxTotalPrice :: Lens.Lens' OnDemandOptions (Core.Maybe Types.String)
odoMaxTotalPrice = Lens.field @"maxTotalPrice"
{-# DEPRECATED odoMaxTotalPrice "Use generic-lens or generic-optics with 'maxTotalPrice' instead." #-}

-- | The minimum target capacity for On-Demand Instances in the fleet. If the minimum target capacity is not reached, the fleet launches no instances.
--
-- /Note:/ Consider using 'minTargetCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odoMinTargetCapacity :: Lens.Lens' OnDemandOptions (Core.Maybe Core.Int)
odoMinTargetCapacity = Lens.field @"minTargetCapacity"
{-# DEPRECATED odoMinTargetCapacity "Use generic-lens or generic-optics with 'minTargetCapacity' instead." #-}

-- | Indicates that the fleet launches all On-Demand Instances into a single Availability Zone. Supported only for fleets of type @instant@ .
--
-- /Note:/ Consider using 'singleAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odoSingleAvailabilityZone :: Lens.Lens' OnDemandOptions (Core.Maybe Core.Bool)
odoSingleAvailabilityZone = Lens.field @"singleAvailabilityZone"
{-# DEPRECATED odoSingleAvailabilityZone "Use generic-lens or generic-optics with 'singleAvailabilityZone' instead." #-}

-- | Indicates that the fleet uses a single instance type to launch all On-Demand Instances in the fleet. Supported only for fleets of type @instant@ .
--
-- /Note:/ Consider using 'singleInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odoSingleInstanceType :: Lens.Lens' OnDemandOptions (Core.Maybe Core.Bool)
odoSingleInstanceType = Lens.field @"singleInstanceType"
{-# DEPRECATED odoSingleInstanceType "Use generic-lens or generic-optics with 'singleInstanceType' instead." #-}

instance Core.FromXML OnDemandOptions where
  parseXML x =
    OnDemandOptions'
      Core.<$> (x Core..@? "allocationStrategy")
      Core.<*> (x Core..@? "capacityReservationOptions")
      Core.<*> (x Core..@? "maxTotalPrice")
      Core.<*> (x Core..@? "minTargetCapacity")
      Core.<*> (x Core..@? "singleAvailabilityZone")
      Core.<*> (x Core..@? "singleInstanceType")
