{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CapacityReservationOptionsRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CapacityReservationOptionsRequest
  ( CapacityReservationOptionsRequest (..),

    -- * Smart constructor
    mkCapacityReservationOptionsRequest,

    -- * Lenses
    crorUsageStrategy,
  )
where

import Network.AWS.EC2.Types.FleetCapacityReservationUsageStrategy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the strategy for using unused Capacity Reservations for fulfilling On-Demand capacity.
--
-- For more information about Capacity Reservations, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-capacity-reservations.html On-Demand Capacity Reservations> in the /Amazon Elastic Compute Cloud User Guide/ . For examples of using Capacity Reservations in an EC2 Fleet, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-fleet-examples.html EC2 Fleet example configurations> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- /See:/ 'mkCapacityReservationOptionsRequest' smart constructor.
newtype CapacityReservationOptionsRequest = CapacityReservationOptionsRequest'
  { -- | Indicates whether to use unused Capacity Reservations for fulfilling On-Demand capacity.
    --
    -- If you specify @use-capacity-reservations-first@ , the fleet uses unused Capacity Reservations to fulfill On-Demand capacity up to the target On-Demand capacity. If multiple instance pools have unused Capacity Reservations, the On-Demand allocation strategy (@lowest-price@ or @prioritized@ ) is applied. If the number of unused Capacity Reservations is less than the On-Demand target capacity, the remaining On-Demand target capacity is launched according to the On-Demand allocation strategy (@lowest-price@ or @prioritized@ ).
    -- If you do not specify a value, the fleet fulfils the On-Demand capacity according to the chosen On-Demand allocation strategy.
    usageStrategy :: Lude.Maybe FleetCapacityReservationUsageStrategy
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CapacityReservationOptionsRequest' with the minimum fields required to make a request.
--
-- * 'usageStrategy' - Indicates whether to use unused Capacity Reservations for fulfilling On-Demand capacity.
--
-- If you specify @use-capacity-reservations-first@ , the fleet uses unused Capacity Reservations to fulfill On-Demand capacity up to the target On-Demand capacity. If multiple instance pools have unused Capacity Reservations, the On-Demand allocation strategy (@lowest-price@ or @prioritized@ ) is applied. If the number of unused Capacity Reservations is less than the On-Demand target capacity, the remaining On-Demand target capacity is launched according to the On-Demand allocation strategy (@lowest-price@ or @prioritized@ ).
-- If you do not specify a value, the fleet fulfils the On-Demand capacity according to the chosen On-Demand allocation strategy.
mkCapacityReservationOptionsRequest ::
  CapacityReservationOptionsRequest
mkCapacityReservationOptionsRequest =
  CapacityReservationOptionsRequest' {usageStrategy = Lude.Nothing}

-- | Indicates whether to use unused Capacity Reservations for fulfilling On-Demand capacity.
--
-- If you specify @use-capacity-reservations-first@ , the fleet uses unused Capacity Reservations to fulfill On-Demand capacity up to the target On-Demand capacity. If multiple instance pools have unused Capacity Reservations, the On-Demand allocation strategy (@lowest-price@ or @prioritized@ ) is applied. If the number of unused Capacity Reservations is less than the On-Demand target capacity, the remaining On-Demand target capacity is launched according to the On-Demand allocation strategy (@lowest-price@ or @prioritized@ ).
-- If you do not specify a value, the fleet fulfils the On-Demand capacity according to the chosen On-Demand allocation strategy.
--
-- /Note:/ Consider using 'usageStrategy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crorUsageStrategy :: Lens.Lens' CapacityReservationOptionsRequest (Lude.Maybe FleetCapacityReservationUsageStrategy)
crorUsageStrategy = Lens.lens (usageStrategy :: CapacityReservationOptionsRequest -> Lude.Maybe FleetCapacityReservationUsageStrategy) (\s a -> s {usageStrategy = a} :: CapacityReservationOptionsRequest)
{-# DEPRECATED crorUsageStrategy "Use generic-lens or generic-optics with 'usageStrategy' instead." #-}

instance Lude.ToQuery CapacityReservationOptionsRequest where
  toQuery CapacityReservationOptionsRequest' {..} =
    Lude.mconcat ["UsageStrategy" Lude.=: usageStrategy]
