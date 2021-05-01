{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.Types.CapacityReservationOptionsRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CapacityReservationOptionsRequest where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.FleetCapacityReservationUsageStrategy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the strategy for using unused Capacity Reservations for
-- fulfilling On-Demand capacity.
--
-- This strategy can only be used if the EC2 Fleet is of type @instant@.
--
-- For more information about Capacity Reservations, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-capacity-reservations.html On-Demand Capacity Reservations>
-- in the /Amazon EC2 User Guide/. For examples of using Capacity
-- Reservations in an EC2 Fleet, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-fleet-examples.html EC2 Fleet example configurations>
-- in the /Amazon EC2 User Guide/.
--
-- /See:/ 'newCapacityReservationOptionsRequest' smart constructor.
data CapacityReservationOptionsRequest = CapacityReservationOptionsRequest'
  { -- | Indicates whether to use unused Capacity Reservations for fulfilling
    -- On-Demand capacity.
    --
    -- If you specify @use-capacity-reservations-first@, the fleet uses unused
    -- Capacity Reservations to fulfill On-Demand capacity up to the target
    -- On-Demand capacity. If multiple instance pools have unused Capacity
    -- Reservations, the On-Demand allocation strategy (@lowest-price@ or
    -- @prioritized@) is applied. If the number of unused Capacity Reservations
    -- is less than the On-Demand target capacity, the remaining On-Demand
    -- target capacity is launched according to the On-Demand allocation
    -- strategy (@lowest-price@ or @prioritized@).
    --
    -- If you do not specify a value, the fleet fulfils the On-Demand capacity
    -- according to the chosen On-Demand allocation strategy.
    usageStrategy :: Prelude.Maybe FleetCapacityReservationUsageStrategy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CapacityReservationOptionsRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'usageStrategy', 'capacityReservationOptionsRequest_usageStrategy' - Indicates whether to use unused Capacity Reservations for fulfilling
-- On-Demand capacity.
--
-- If you specify @use-capacity-reservations-first@, the fleet uses unused
-- Capacity Reservations to fulfill On-Demand capacity up to the target
-- On-Demand capacity. If multiple instance pools have unused Capacity
-- Reservations, the On-Demand allocation strategy (@lowest-price@ or
-- @prioritized@) is applied. If the number of unused Capacity Reservations
-- is less than the On-Demand target capacity, the remaining On-Demand
-- target capacity is launched according to the On-Demand allocation
-- strategy (@lowest-price@ or @prioritized@).
--
-- If you do not specify a value, the fleet fulfils the On-Demand capacity
-- according to the chosen On-Demand allocation strategy.
newCapacityReservationOptionsRequest ::
  CapacityReservationOptionsRequest
newCapacityReservationOptionsRequest =
  CapacityReservationOptionsRequest'
    { usageStrategy =
        Prelude.Nothing
    }

-- | Indicates whether to use unused Capacity Reservations for fulfilling
-- On-Demand capacity.
--
-- If you specify @use-capacity-reservations-first@, the fleet uses unused
-- Capacity Reservations to fulfill On-Demand capacity up to the target
-- On-Demand capacity. If multiple instance pools have unused Capacity
-- Reservations, the On-Demand allocation strategy (@lowest-price@ or
-- @prioritized@) is applied. If the number of unused Capacity Reservations
-- is less than the On-Demand target capacity, the remaining On-Demand
-- target capacity is launched according to the On-Demand allocation
-- strategy (@lowest-price@ or @prioritized@).
--
-- If you do not specify a value, the fleet fulfils the On-Demand capacity
-- according to the chosen On-Demand allocation strategy.
capacityReservationOptionsRequest_usageStrategy :: Lens.Lens' CapacityReservationOptionsRequest (Prelude.Maybe FleetCapacityReservationUsageStrategy)
capacityReservationOptionsRequest_usageStrategy = Lens.lens (\CapacityReservationOptionsRequest' {usageStrategy} -> usageStrategy) (\s@CapacityReservationOptionsRequest' {} a -> s {usageStrategy = a} :: CapacityReservationOptionsRequest)

instance
  Prelude.Hashable
    CapacityReservationOptionsRequest

instance
  Prelude.NFData
    CapacityReservationOptionsRequest

instance
  Prelude.ToQuery
    CapacityReservationOptionsRequest
  where
  toQuery CapacityReservationOptionsRequest' {..} =
    Prelude.mconcat
      ["UsageStrategy" Prelude.=: usageStrategy]
