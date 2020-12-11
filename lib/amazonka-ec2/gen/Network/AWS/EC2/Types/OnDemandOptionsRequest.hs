-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.OnDemandOptionsRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.OnDemandOptionsRequest
  ( OnDemandOptionsRequest (..),

    -- * Smart constructor
    mkOnDemandOptionsRequest,

    -- * Lenses
    odorCapacityReservationOptions,
    odorSingleAvailabilityZone,
    odorMaxTotalPrice,
    odorMinTargetCapacity,
    odorSingleInstanceType,
    odorAllocationStrategy,
  )
where

import Network.AWS.EC2.Types.CapacityReservationOptionsRequest
import Network.AWS.EC2.Types.FleetOnDemandAllocationStrategy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the configuration of On-Demand Instances in an EC2 Fleet.
--
-- /See:/ 'mkOnDemandOptionsRequest' smart constructor.
data OnDemandOptionsRequest = OnDemandOptionsRequest'
  { capacityReservationOptions ::
      Lude.Maybe CapacityReservationOptionsRequest,
    singleAvailabilityZone ::
      Lude.Maybe Lude.Bool,
    maxTotalPrice :: Lude.Maybe Lude.Text,
    minTargetCapacity :: Lude.Maybe Lude.Int,
    singleInstanceType :: Lude.Maybe Lude.Bool,
    allocationStrategy ::
      Lude.Maybe FleetOnDemandAllocationStrategy
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OnDemandOptionsRequest' with the minimum fields required to make a request.
--
-- * 'allocationStrategy' - The order of the launch template overrides to use in fulfilling On-Demand capacity. If you specify @lowest-price@ , EC2 Fleet uses price to determine the order, launching the lowest price first. If you specify @prioritized@ , EC2 Fleet uses the priority that you assigned to each launch template override, launching the highest priority first. If you do not specify a value, EC2 Fleet defaults to @lowest-price@ .
-- * 'capacityReservationOptions' - The strategy for using unused Capacity Reservations for fulfilling On-Demand capacity. Supported only for fleets of type @instant@ .
-- * 'maxTotalPrice' - The maximum amount per hour for On-Demand Instances that you're willing to pay.
-- * 'minTargetCapacity' - The minimum target capacity for On-Demand Instances in the fleet. If the minimum target capacity is not reached, the fleet launches no instances.
-- * 'singleAvailabilityZone' - Indicates that the fleet launches all On-Demand Instances into a single Availability Zone. Supported only for fleets of type @instant@ .
-- * 'singleInstanceType' - Indicates that the fleet uses a single instance type to launch all On-Demand Instances in the fleet. Supported only for fleets of type @instant@ .
mkOnDemandOptionsRequest ::
  OnDemandOptionsRequest
mkOnDemandOptionsRequest =
  OnDemandOptionsRequest'
    { capacityReservationOptions =
        Lude.Nothing,
      singleAvailabilityZone = Lude.Nothing,
      maxTotalPrice = Lude.Nothing,
      minTargetCapacity = Lude.Nothing,
      singleInstanceType = Lude.Nothing,
      allocationStrategy = Lude.Nothing
    }

-- | The strategy for using unused Capacity Reservations for fulfilling On-Demand capacity. Supported only for fleets of type @instant@ .
--
-- /Note:/ Consider using 'capacityReservationOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odorCapacityReservationOptions :: Lens.Lens' OnDemandOptionsRequest (Lude.Maybe CapacityReservationOptionsRequest)
odorCapacityReservationOptions = Lens.lens (capacityReservationOptions :: OnDemandOptionsRequest -> Lude.Maybe CapacityReservationOptionsRequest) (\s a -> s {capacityReservationOptions = a} :: OnDemandOptionsRequest)
{-# DEPRECATED odorCapacityReservationOptions "Use generic-lens or generic-optics with 'capacityReservationOptions' instead." #-}

-- | Indicates that the fleet launches all On-Demand Instances into a single Availability Zone. Supported only for fleets of type @instant@ .
--
-- /Note:/ Consider using 'singleAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odorSingleAvailabilityZone :: Lens.Lens' OnDemandOptionsRequest (Lude.Maybe Lude.Bool)
odorSingleAvailabilityZone = Lens.lens (singleAvailabilityZone :: OnDemandOptionsRequest -> Lude.Maybe Lude.Bool) (\s a -> s {singleAvailabilityZone = a} :: OnDemandOptionsRequest)
{-# DEPRECATED odorSingleAvailabilityZone "Use generic-lens or generic-optics with 'singleAvailabilityZone' instead." #-}

-- | The maximum amount per hour for On-Demand Instances that you're willing to pay.
--
-- /Note:/ Consider using 'maxTotalPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odorMaxTotalPrice :: Lens.Lens' OnDemandOptionsRequest (Lude.Maybe Lude.Text)
odorMaxTotalPrice = Lens.lens (maxTotalPrice :: OnDemandOptionsRequest -> Lude.Maybe Lude.Text) (\s a -> s {maxTotalPrice = a} :: OnDemandOptionsRequest)
{-# DEPRECATED odorMaxTotalPrice "Use generic-lens or generic-optics with 'maxTotalPrice' instead." #-}

-- | The minimum target capacity for On-Demand Instances in the fleet. If the minimum target capacity is not reached, the fleet launches no instances.
--
-- /Note:/ Consider using 'minTargetCapacity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odorMinTargetCapacity :: Lens.Lens' OnDemandOptionsRequest (Lude.Maybe Lude.Int)
odorMinTargetCapacity = Lens.lens (minTargetCapacity :: OnDemandOptionsRequest -> Lude.Maybe Lude.Int) (\s a -> s {minTargetCapacity = a} :: OnDemandOptionsRequest)
{-# DEPRECATED odorMinTargetCapacity "Use generic-lens or generic-optics with 'minTargetCapacity' instead." #-}

-- | Indicates that the fleet uses a single instance type to launch all On-Demand Instances in the fleet. Supported only for fleets of type @instant@ .
--
-- /Note:/ Consider using 'singleInstanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odorSingleInstanceType :: Lens.Lens' OnDemandOptionsRequest (Lude.Maybe Lude.Bool)
odorSingleInstanceType = Lens.lens (singleInstanceType :: OnDemandOptionsRequest -> Lude.Maybe Lude.Bool) (\s a -> s {singleInstanceType = a} :: OnDemandOptionsRequest)
{-# DEPRECATED odorSingleInstanceType "Use generic-lens or generic-optics with 'singleInstanceType' instead." #-}

-- | The order of the launch template overrides to use in fulfilling On-Demand capacity. If you specify @lowest-price@ , EC2 Fleet uses price to determine the order, launching the lowest price first. If you specify @prioritized@ , EC2 Fleet uses the priority that you assigned to each launch template override, launching the highest priority first. If you do not specify a value, EC2 Fleet defaults to @lowest-price@ .
--
-- /Note:/ Consider using 'allocationStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odorAllocationStrategy :: Lens.Lens' OnDemandOptionsRequest (Lude.Maybe FleetOnDemandAllocationStrategy)
odorAllocationStrategy = Lens.lens (allocationStrategy :: OnDemandOptionsRequest -> Lude.Maybe FleetOnDemandAllocationStrategy) (\s a -> s {allocationStrategy = a} :: OnDemandOptionsRequest)
{-# DEPRECATED odorAllocationStrategy "Use generic-lens or generic-optics with 'allocationStrategy' instead." #-}

instance Lude.ToQuery OnDemandOptionsRequest where
  toQuery OnDemandOptionsRequest' {..} =
    Lude.mconcat
      [ "CapacityReservationOptions" Lude.=: capacityReservationOptions,
        "SingleAvailabilityZone" Lude.=: singleAvailabilityZone,
        "MaxTotalPrice" Lude.=: maxTotalPrice,
        "MinTargetCapacity" Lude.=: minTargetCapacity,
        "SingleInstanceType" Lude.=: singleInstanceType,
        "AllocationStrategy" Lude.=: allocationStrategy
      ]
