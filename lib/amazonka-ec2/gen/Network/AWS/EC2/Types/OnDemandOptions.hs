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
    odoCapacityReservationOptions,
    odoSingleAvailabilityZone,
    odoMaxTotalPrice,
    odoMinTargetCapacity,
    odoSingleInstanceType,
    odoAllocationStrategy,
  )
where

import Network.AWS.EC2.Types.CapacityReservationOptions
import Network.AWS.EC2.Types.FleetOnDemandAllocationStrategy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the configuration of On-Demand Instances in an EC2 Fleet.
--
-- /See:/ 'mkOnDemandOptions' smart constructor.
data OnDemandOptions = OnDemandOptions'
  { -- | The strategy for using unused Capacity Reservations for fulfilling On-Demand capacity. Supported only for fleets of type @instant@ .
    capacityReservationOptions :: Lude.Maybe CapacityReservationOptions,
    -- | Indicates that the fleet launches all On-Demand Instances into a single Availability Zone. Supported only for fleets of type @instant@ .
    singleAvailabilityZone :: Lude.Maybe Lude.Bool,
    -- | The maximum amount per hour for On-Demand Instances that you're willing to pay.
    maxTotalPrice :: Lude.Maybe Lude.Text,
    -- | The minimum target capacity for On-Demand Instances in the fleet. If the minimum target capacity is not reached, the fleet launches no instances.
    minTargetCapacity :: Lude.Maybe Lude.Int,
    -- | Indicates that the fleet uses a single instance type to launch all On-Demand Instances in the fleet. Supported only for fleets of type @instant@ .
    singleInstanceType :: Lude.Maybe Lude.Bool,
    -- | The order of the launch template overrides to use in fulfilling On-Demand capacity. If you specify @lowest-price@ , EC2 Fleet uses price to determine the order, launching the lowest price first. If you specify @prioritized@ , EC2 Fleet uses the priority that you assigned to each launch template override, launching the highest priority first. If you do not specify a value, EC2 Fleet defaults to @lowest-price@ .
    allocationStrategy :: Lude.Maybe FleetOnDemandAllocationStrategy
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OnDemandOptions' with the minimum fields required to make a request.
--
-- * 'capacityReservationOptions' - The strategy for using unused Capacity Reservations for fulfilling On-Demand capacity. Supported only for fleets of type @instant@ .
-- * 'singleAvailabilityZone' - Indicates that the fleet launches all On-Demand Instances into a single Availability Zone. Supported only for fleets of type @instant@ .
-- * 'maxTotalPrice' - The maximum amount per hour for On-Demand Instances that you're willing to pay.
-- * 'minTargetCapacity' - The minimum target capacity for On-Demand Instances in the fleet. If the minimum target capacity is not reached, the fleet launches no instances.
-- * 'singleInstanceType' - Indicates that the fleet uses a single instance type to launch all On-Demand Instances in the fleet. Supported only for fleets of type @instant@ .
-- * 'allocationStrategy' - The order of the launch template overrides to use in fulfilling On-Demand capacity. If you specify @lowest-price@ , EC2 Fleet uses price to determine the order, launching the lowest price first. If you specify @prioritized@ , EC2 Fleet uses the priority that you assigned to each launch template override, launching the highest priority first. If you do not specify a value, EC2 Fleet defaults to @lowest-price@ .
mkOnDemandOptions ::
  OnDemandOptions
mkOnDemandOptions =
  OnDemandOptions'
    { capacityReservationOptions = Lude.Nothing,
      singleAvailabilityZone = Lude.Nothing,
      maxTotalPrice = Lude.Nothing,
      minTargetCapacity = Lude.Nothing,
      singleInstanceType = Lude.Nothing,
      allocationStrategy = Lude.Nothing
    }

-- | The strategy for using unused Capacity Reservations for fulfilling On-Demand capacity. Supported only for fleets of type @instant@ .
--
-- /Note:/ Consider using 'capacityReservationOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odoCapacityReservationOptions :: Lens.Lens' OnDemandOptions (Lude.Maybe CapacityReservationOptions)
odoCapacityReservationOptions = Lens.lens (capacityReservationOptions :: OnDemandOptions -> Lude.Maybe CapacityReservationOptions) (\s a -> s {capacityReservationOptions = a} :: OnDemandOptions)
{-# DEPRECATED odoCapacityReservationOptions "Use generic-lens or generic-optics with 'capacityReservationOptions' instead." #-}

-- | Indicates that the fleet launches all On-Demand Instances into a single Availability Zone. Supported only for fleets of type @instant@ .
--
-- /Note:/ Consider using 'singleAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odoSingleAvailabilityZone :: Lens.Lens' OnDemandOptions (Lude.Maybe Lude.Bool)
odoSingleAvailabilityZone = Lens.lens (singleAvailabilityZone :: OnDemandOptions -> Lude.Maybe Lude.Bool) (\s a -> s {singleAvailabilityZone = a} :: OnDemandOptions)
{-# DEPRECATED odoSingleAvailabilityZone "Use generic-lens or generic-optics with 'singleAvailabilityZone' instead." #-}

-- | The maximum amount per hour for On-Demand Instances that you're willing to pay.
--
-- /Note:/ Consider using 'maxTotalPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odoMaxTotalPrice :: Lens.Lens' OnDemandOptions (Lude.Maybe Lude.Text)
odoMaxTotalPrice = Lens.lens (maxTotalPrice :: OnDemandOptions -> Lude.Maybe Lude.Text) (\s a -> s {maxTotalPrice = a} :: OnDemandOptions)
{-# DEPRECATED odoMaxTotalPrice "Use generic-lens or generic-optics with 'maxTotalPrice' instead." #-}

-- | The minimum target capacity for On-Demand Instances in the fleet. If the minimum target capacity is not reached, the fleet launches no instances.
--
-- /Note:/ Consider using 'minTargetCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odoMinTargetCapacity :: Lens.Lens' OnDemandOptions (Lude.Maybe Lude.Int)
odoMinTargetCapacity = Lens.lens (minTargetCapacity :: OnDemandOptions -> Lude.Maybe Lude.Int) (\s a -> s {minTargetCapacity = a} :: OnDemandOptions)
{-# DEPRECATED odoMinTargetCapacity "Use generic-lens or generic-optics with 'minTargetCapacity' instead." #-}

-- | Indicates that the fleet uses a single instance type to launch all On-Demand Instances in the fleet. Supported only for fleets of type @instant@ .
--
-- /Note:/ Consider using 'singleInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odoSingleInstanceType :: Lens.Lens' OnDemandOptions (Lude.Maybe Lude.Bool)
odoSingleInstanceType = Lens.lens (singleInstanceType :: OnDemandOptions -> Lude.Maybe Lude.Bool) (\s a -> s {singleInstanceType = a} :: OnDemandOptions)
{-# DEPRECATED odoSingleInstanceType "Use generic-lens or generic-optics with 'singleInstanceType' instead." #-}

-- | The order of the launch template overrides to use in fulfilling On-Demand capacity. If you specify @lowest-price@ , EC2 Fleet uses price to determine the order, launching the lowest price first. If you specify @prioritized@ , EC2 Fleet uses the priority that you assigned to each launch template override, launching the highest priority first. If you do not specify a value, EC2 Fleet defaults to @lowest-price@ .
--
-- /Note:/ Consider using 'allocationStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odoAllocationStrategy :: Lens.Lens' OnDemandOptions (Lude.Maybe FleetOnDemandAllocationStrategy)
odoAllocationStrategy = Lens.lens (allocationStrategy :: OnDemandOptions -> Lude.Maybe FleetOnDemandAllocationStrategy) (\s a -> s {allocationStrategy = a} :: OnDemandOptions)
{-# DEPRECATED odoAllocationStrategy "Use generic-lens or generic-optics with 'allocationStrategy' instead." #-}

instance Lude.FromXML OnDemandOptions where
  parseXML x =
    OnDemandOptions'
      Lude.<$> (x Lude..@? "capacityReservationOptions")
      Lude.<*> (x Lude..@? "singleAvailabilityZone")
      Lude.<*> (x Lude..@? "maxTotalPrice")
      Lude.<*> (x Lude..@? "minTargetCapacity")
      Lude.<*> (x Lude..@? "singleInstanceType")
      Lude.<*> (x Lude..@? "allocationStrategy")
