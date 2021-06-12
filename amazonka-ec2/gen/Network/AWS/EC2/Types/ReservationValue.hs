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
-- Module      : Network.AWS.EC2.Types.ReservationValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ReservationValue where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | The cost associated with the Reserved Instance.
--
-- /See:/ 'newReservationValue' smart constructor.
data ReservationValue = ReservationValue'
  { -- | The remaining upfront cost of the reservation.
    remainingUpfrontValue :: Core.Maybe Core.Text,
    -- | The hourly rate of the reservation.
    hourlyPrice :: Core.Maybe Core.Text,
    -- | The balance of the total value (the sum of remainingUpfrontValue +
    -- hourlyPrice * number of hours remaining).
    remainingTotalValue :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ReservationValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'remainingUpfrontValue', 'reservationValue_remainingUpfrontValue' - The remaining upfront cost of the reservation.
--
-- 'hourlyPrice', 'reservationValue_hourlyPrice' - The hourly rate of the reservation.
--
-- 'remainingTotalValue', 'reservationValue_remainingTotalValue' - The balance of the total value (the sum of remainingUpfrontValue +
-- hourlyPrice * number of hours remaining).
newReservationValue ::
  ReservationValue
newReservationValue =
  ReservationValue'
    { remainingUpfrontValue =
        Core.Nothing,
      hourlyPrice = Core.Nothing,
      remainingTotalValue = Core.Nothing
    }

-- | The remaining upfront cost of the reservation.
reservationValue_remainingUpfrontValue :: Lens.Lens' ReservationValue (Core.Maybe Core.Text)
reservationValue_remainingUpfrontValue = Lens.lens (\ReservationValue' {remainingUpfrontValue} -> remainingUpfrontValue) (\s@ReservationValue' {} a -> s {remainingUpfrontValue = a} :: ReservationValue)

-- | The hourly rate of the reservation.
reservationValue_hourlyPrice :: Lens.Lens' ReservationValue (Core.Maybe Core.Text)
reservationValue_hourlyPrice = Lens.lens (\ReservationValue' {hourlyPrice} -> hourlyPrice) (\s@ReservationValue' {} a -> s {hourlyPrice = a} :: ReservationValue)

-- | The balance of the total value (the sum of remainingUpfrontValue +
-- hourlyPrice * number of hours remaining).
reservationValue_remainingTotalValue :: Lens.Lens' ReservationValue (Core.Maybe Core.Text)
reservationValue_remainingTotalValue = Lens.lens (\ReservationValue' {remainingTotalValue} -> remainingTotalValue) (\s@ReservationValue' {} a -> s {remainingTotalValue = a} :: ReservationValue)

instance Core.FromXML ReservationValue where
  parseXML x =
    ReservationValue'
      Core.<$> (x Core..@? "remainingUpfrontValue")
      Core.<*> (x Core..@? "hourlyPrice")
      Core.<*> (x Core..@? "remainingTotalValue")

instance Core.Hashable ReservationValue

instance Core.NFData ReservationValue
