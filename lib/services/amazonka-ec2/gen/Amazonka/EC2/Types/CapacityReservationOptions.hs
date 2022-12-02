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
-- Module      : Amazonka.EC2.Types.CapacityReservationOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.CapacityReservationOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.FleetCapacityReservationUsageStrategy
import qualified Amazonka.Prelude as Prelude

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
-- /See:/ 'newCapacityReservationOptions' smart constructor.
data CapacityReservationOptions = CapacityReservationOptions'
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CapacityReservationOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'usageStrategy', 'capacityReservationOptions_usageStrategy' - Indicates whether to use unused Capacity Reservations for fulfilling
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
newCapacityReservationOptions ::
  CapacityReservationOptions
newCapacityReservationOptions =
  CapacityReservationOptions'
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
capacityReservationOptions_usageStrategy :: Lens.Lens' CapacityReservationOptions (Prelude.Maybe FleetCapacityReservationUsageStrategy)
capacityReservationOptions_usageStrategy = Lens.lens (\CapacityReservationOptions' {usageStrategy} -> usageStrategy) (\s@CapacityReservationOptions' {} a -> s {usageStrategy = a} :: CapacityReservationOptions)

instance Data.FromXML CapacityReservationOptions where
  parseXML x =
    CapacityReservationOptions'
      Prelude.<$> (x Data..@? "usageStrategy")

instance Prelude.Hashable CapacityReservationOptions where
  hashWithSalt _salt CapacityReservationOptions' {..} =
    _salt `Prelude.hashWithSalt` usageStrategy

instance Prelude.NFData CapacityReservationOptions where
  rnf CapacityReservationOptions' {..} =
    Prelude.rnf usageStrategy
