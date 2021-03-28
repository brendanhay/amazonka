{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.OnDemandOptionsRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.OnDemandOptionsRequest
  ( OnDemandOptionsRequest (..)
  -- * Smart constructor
  , mkOnDemandOptionsRequest
  -- * Lenses
  , odorAllocationStrategy
  , odorCapacityReservationOptions
  , odorMaxTotalPrice
  , odorMinTargetCapacity
  , odorSingleAvailabilityZone
  , odorSingleInstanceType
  ) where

import qualified Network.AWS.EC2.Types.CapacityReservationOptionsRequest as Types
import qualified Network.AWS.EC2.Types.FleetOnDemandAllocationStrategy as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the configuration of On-Demand Instances in an EC2 Fleet.
--
-- /See:/ 'mkOnDemandOptionsRequest' smart constructor.
data OnDemandOptionsRequest = OnDemandOptionsRequest'
  { allocationStrategy :: Core.Maybe Types.FleetOnDemandAllocationStrategy
    -- ^ The order of the launch template overrides to use in fulfilling On-Demand capacity. If you specify @lowest-price@ , EC2 Fleet uses price to determine the order, launching the lowest price first. If you specify @prioritized@ , EC2 Fleet uses the priority that you assigned to each launch template override, launching the highest priority first. If you do not specify a value, EC2 Fleet defaults to @lowest-price@ .
  , capacityReservationOptions :: Core.Maybe Types.CapacityReservationOptionsRequest
    -- ^ The strategy for using unused Capacity Reservations for fulfilling On-Demand capacity. Supported only for fleets of type @instant@ .
  , maxTotalPrice :: Core.Maybe Core.Text
    -- ^ The maximum amount per hour for On-Demand Instances that you're willing to pay.
  , minTargetCapacity :: Core.Maybe Core.Int
    -- ^ The minimum target capacity for On-Demand Instances in the fleet. If the minimum target capacity is not reached, the fleet launches no instances.
  , singleAvailabilityZone :: Core.Maybe Core.Bool
    -- ^ Indicates that the fleet launches all On-Demand Instances into a single Availability Zone. Supported only for fleets of type @instant@ .
  , singleInstanceType :: Core.Maybe Core.Bool
    -- ^ Indicates that the fleet uses a single instance type to launch all On-Demand Instances in the fleet. Supported only for fleets of type @instant@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OnDemandOptionsRequest' value with any optional fields omitted.
mkOnDemandOptionsRequest
    :: OnDemandOptionsRequest
mkOnDemandOptionsRequest
  = OnDemandOptionsRequest'{allocationStrategy = Core.Nothing,
                            capacityReservationOptions = Core.Nothing,
                            maxTotalPrice = Core.Nothing, minTargetCapacity = Core.Nothing,
                            singleAvailabilityZone = Core.Nothing,
                            singleInstanceType = Core.Nothing}

-- | The order of the launch template overrides to use in fulfilling On-Demand capacity. If you specify @lowest-price@ , EC2 Fleet uses price to determine the order, launching the lowest price first. If you specify @prioritized@ , EC2 Fleet uses the priority that you assigned to each launch template override, launching the highest priority first. If you do not specify a value, EC2 Fleet defaults to @lowest-price@ .
--
-- /Note:/ Consider using 'allocationStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odorAllocationStrategy :: Lens.Lens' OnDemandOptionsRequest (Core.Maybe Types.FleetOnDemandAllocationStrategy)
odorAllocationStrategy = Lens.field @"allocationStrategy"
{-# INLINEABLE odorAllocationStrategy #-}
{-# DEPRECATED allocationStrategy "Use generic-lens or generic-optics with 'allocationStrategy' instead"  #-}

-- | The strategy for using unused Capacity Reservations for fulfilling On-Demand capacity. Supported only for fleets of type @instant@ .
--
-- /Note:/ Consider using 'capacityReservationOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odorCapacityReservationOptions :: Lens.Lens' OnDemandOptionsRequest (Core.Maybe Types.CapacityReservationOptionsRequest)
odorCapacityReservationOptions = Lens.field @"capacityReservationOptions"
{-# INLINEABLE odorCapacityReservationOptions #-}
{-# DEPRECATED capacityReservationOptions "Use generic-lens or generic-optics with 'capacityReservationOptions' instead"  #-}

-- | The maximum amount per hour for On-Demand Instances that you're willing to pay.
--
-- /Note:/ Consider using 'maxTotalPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odorMaxTotalPrice :: Lens.Lens' OnDemandOptionsRequest (Core.Maybe Core.Text)
odorMaxTotalPrice = Lens.field @"maxTotalPrice"
{-# INLINEABLE odorMaxTotalPrice #-}
{-# DEPRECATED maxTotalPrice "Use generic-lens or generic-optics with 'maxTotalPrice' instead"  #-}

-- | The minimum target capacity for On-Demand Instances in the fleet. If the minimum target capacity is not reached, the fleet launches no instances.
--
-- /Note:/ Consider using 'minTargetCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odorMinTargetCapacity :: Lens.Lens' OnDemandOptionsRequest (Core.Maybe Core.Int)
odorMinTargetCapacity = Lens.field @"minTargetCapacity"
{-# INLINEABLE odorMinTargetCapacity #-}
{-# DEPRECATED minTargetCapacity "Use generic-lens or generic-optics with 'minTargetCapacity' instead"  #-}

-- | Indicates that the fleet launches all On-Demand Instances into a single Availability Zone. Supported only for fleets of type @instant@ .
--
-- /Note:/ Consider using 'singleAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odorSingleAvailabilityZone :: Lens.Lens' OnDemandOptionsRequest (Core.Maybe Core.Bool)
odorSingleAvailabilityZone = Lens.field @"singleAvailabilityZone"
{-# INLINEABLE odorSingleAvailabilityZone #-}
{-# DEPRECATED singleAvailabilityZone "Use generic-lens or generic-optics with 'singleAvailabilityZone' instead"  #-}

-- | Indicates that the fleet uses a single instance type to launch all On-Demand Instances in the fleet. Supported only for fleets of type @instant@ .
--
-- /Note:/ Consider using 'singleInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odorSingleInstanceType :: Lens.Lens' OnDemandOptionsRequest (Core.Maybe Core.Bool)
odorSingleInstanceType = Lens.field @"singleInstanceType"
{-# INLINEABLE odorSingleInstanceType #-}
{-# DEPRECATED singleInstanceType "Use generic-lens or generic-optics with 'singleInstanceType' instead"  #-}

instance Core.ToQuery OnDemandOptionsRequest where
        toQuery OnDemandOptionsRequest{..}
          = Core.maybe Core.mempty (Core.toQueryPair "AllocationStrategy")
              allocationStrategy
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "CapacityReservationOptions")
                capacityReservationOptions
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxTotalPrice")
                maxTotalPrice
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MinTargetCapacity")
                minTargetCapacity
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SingleAvailabilityZone")
                singleAvailabilityZone
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SingleInstanceType")
                singleInstanceType
