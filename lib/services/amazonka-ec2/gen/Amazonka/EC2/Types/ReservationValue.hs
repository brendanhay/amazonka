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
-- Module      : Amazonka.EC2.Types.ReservationValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ReservationValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | The cost associated with the Reserved Instance.
--
-- /See:/ 'newReservationValue' smart constructor.
data ReservationValue = ReservationValue'
  { -- | The hourly rate of the reservation.
    hourlyPrice :: Prelude.Maybe Prelude.Text,
    -- | The balance of the total value (the sum of remainingUpfrontValue +
    -- hourlyPrice * number of hours remaining).
    remainingTotalValue :: Prelude.Maybe Prelude.Text,
    -- | The remaining upfront cost of the reservation.
    remainingUpfrontValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReservationValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hourlyPrice', 'reservationValue_hourlyPrice' - The hourly rate of the reservation.
--
-- 'remainingTotalValue', 'reservationValue_remainingTotalValue' - The balance of the total value (the sum of remainingUpfrontValue +
-- hourlyPrice * number of hours remaining).
--
-- 'remainingUpfrontValue', 'reservationValue_remainingUpfrontValue' - The remaining upfront cost of the reservation.
newReservationValue ::
  ReservationValue
newReservationValue =
  ReservationValue'
    { hourlyPrice = Prelude.Nothing,
      remainingTotalValue = Prelude.Nothing,
      remainingUpfrontValue = Prelude.Nothing
    }

-- | The hourly rate of the reservation.
reservationValue_hourlyPrice :: Lens.Lens' ReservationValue (Prelude.Maybe Prelude.Text)
reservationValue_hourlyPrice = Lens.lens (\ReservationValue' {hourlyPrice} -> hourlyPrice) (\s@ReservationValue' {} a -> s {hourlyPrice = a} :: ReservationValue)

-- | The balance of the total value (the sum of remainingUpfrontValue +
-- hourlyPrice * number of hours remaining).
reservationValue_remainingTotalValue :: Lens.Lens' ReservationValue (Prelude.Maybe Prelude.Text)
reservationValue_remainingTotalValue = Lens.lens (\ReservationValue' {remainingTotalValue} -> remainingTotalValue) (\s@ReservationValue' {} a -> s {remainingTotalValue = a} :: ReservationValue)

-- | The remaining upfront cost of the reservation.
reservationValue_remainingUpfrontValue :: Lens.Lens' ReservationValue (Prelude.Maybe Prelude.Text)
reservationValue_remainingUpfrontValue = Lens.lens (\ReservationValue' {remainingUpfrontValue} -> remainingUpfrontValue) (\s@ReservationValue' {} a -> s {remainingUpfrontValue = a} :: ReservationValue)

instance Data.FromXML ReservationValue where
  parseXML x =
    ReservationValue'
      Prelude.<$> (x Data..@? "hourlyPrice")
      Prelude.<*> (x Data..@? "remainingTotalValue")
      Prelude.<*> (x Data..@? "remainingUpfrontValue")

instance Prelude.Hashable ReservationValue where
  hashWithSalt _salt ReservationValue' {..} =
    _salt `Prelude.hashWithSalt` hourlyPrice
      `Prelude.hashWithSalt` remainingTotalValue
      `Prelude.hashWithSalt` remainingUpfrontValue

instance Prelude.NFData ReservationValue where
  rnf ReservationValue' {..} =
    Prelude.rnf hourlyPrice
      `Prelude.seq` Prelude.rnf remainingTotalValue
      `Prelude.seq` Prelude.rnf remainingUpfrontValue
