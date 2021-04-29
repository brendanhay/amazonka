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
-- Module      : Network.AWS.EC2.Types.ReservationValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ReservationValue where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The cost associated with the Reserved Instance.
--
-- /See:/ 'newReservationValue' smart constructor.
data ReservationValue = ReservationValue'
  { -- | The remaining upfront cost of the reservation.
    remainingUpfrontValue :: Prelude.Maybe Prelude.Text,
    -- | The hourly rate of the reservation.
    hourlyPrice :: Prelude.Maybe Prelude.Text,
    -- | The balance of the total value (the sum of remainingUpfrontValue +
    -- hourlyPrice * number of hours remaining).
    remainingTotalValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      hourlyPrice = Prelude.Nothing,
      remainingTotalValue = Prelude.Nothing
    }

-- | The remaining upfront cost of the reservation.
reservationValue_remainingUpfrontValue :: Lens.Lens' ReservationValue (Prelude.Maybe Prelude.Text)
reservationValue_remainingUpfrontValue = Lens.lens (\ReservationValue' {remainingUpfrontValue} -> remainingUpfrontValue) (\s@ReservationValue' {} a -> s {remainingUpfrontValue = a} :: ReservationValue)

-- | The hourly rate of the reservation.
reservationValue_hourlyPrice :: Lens.Lens' ReservationValue (Prelude.Maybe Prelude.Text)
reservationValue_hourlyPrice = Lens.lens (\ReservationValue' {hourlyPrice} -> hourlyPrice) (\s@ReservationValue' {} a -> s {hourlyPrice = a} :: ReservationValue)

-- | The balance of the total value (the sum of remainingUpfrontValue +
-- hourlyPrice * number of hours remaining).
reservationValue_remainingTotalValue :: Lens.Lens' ReservationValue (Prelude.Maybe Prelude.Text)
reservationValue_remainingTotalValue = Lens.lens (\ReservationValue' {remainingTotalValue} -> remainingTotalValue) (\s@ReservationValue' {} a -> s {remainingTotalValue = a} :: ReservationValue)

instance Prelude.FromXML ReservationValue where
  parseXML x =
    ReservationValue'
      Prelude.<$> (x Prelude..@? "remainingUpfrontValue")
      Prelude.<*> (x Prelude..@? "hourlyPrice")
      Prelude.<*> (x Prelude..@? "remainingTotalValue")

instance Prelude.Hashable ReservationValue

instance Prelude.NFData ReservationValue
