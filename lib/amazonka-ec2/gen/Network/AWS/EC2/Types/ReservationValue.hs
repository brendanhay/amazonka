{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ReservationValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ReservationValue
  ( ReservationValue (..),

    -- * Smart constructor
    mkReservationValue,

    -- * Lenses
    rvHourlyPrice,
    rvRemainingTotalValue,
    rvRemainingUpfrontValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The cost associated with the Reserved Instance.
--
-- /See:/ 'mkReservationValue' smart constructor.
data ReservationValue = ReservationValue'
  { -- | The hourly rate of the reservation.
    hourlyPrice :: Lude.Maybe Lude.Text,
    -- | The balance of the total value (the sum of remainingUpfrontValue + hourlyPrice * number of hours remaining).
    remainingTotalValue :: Lude.Maybe Lude.Text,
    -- | The remaining upfront cost of the reservation.
    remainingUpfrontValue :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReservationValue' with the minimum fields required to make a request.
--
-- * 'hourlyPrice' - The hourly rate of the reservation.
-- * 'remainingTotalValue' - The balance of the total value (the sum of remainingUpfrontValue + hourlyPrice * number of hours remaining).
-- * 'remainingUpfrontValue' - The remaining upfront cost of the reservation.
mkReservationValue ::
  ReservationValue
mkReservationValue =
  ReservationValue'
    { hourlyPrice = Lude.Nothing,
      remainingTotalValue = Lude.Nothing,
      remainingUpfrontValue = Lude.Nothing
    }

-- | The hourly rate of the reservation.
--
-- /Note:/ Consider using 'hourlyPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvHourlyPrice :: Lens.Lens' ReservationValue (Lude.Maybe Lude.Text)
rvHourlyPrice = Lens.lens (hourlyPrice :: ReservationValue -> Lude.Maybe Lude.Text) (\s a -> s {hourlyPrice = a} :: ReservationValue)
{-# DEPRECATED rvHourlyPrice "Use generic-lens or generic-optics with 'hourlyPrice' instead." #-}

-- | The balance of the total value (the sum of remainingUpfrontValue + hourlyPrice * number of hours remaining).
--
-- /Note:/ Consider using 'remainingTotalValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvRemainingTotalValue :: Lens.Lens' ReservationValue (Lude.Maybe Lude.Text)
rvRemainingTotalValue = Lens.lens (remainingTotalValue :: ReservationValue -> Lude.Maybe Lude.Text) (\s a -> s {remainingTotalValue = a} :: ReservationValue)
{-# DEPRECATED rvRemainingTotalValue "Use generic-lens or generic-optics with 'remainingTotalValue' instead." #-}

-- | The remaining upfront cost of the reservation.
--
-- /Note:/ Consider using 'remainingUpfrontValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvRemainingUpfrontValue :: Lens.Lens' ReservationValue (Lude.Maybe Lude.Text)
rvRemainingUpfrontValue = Lens.lens (remainingUpfrontValue :: ReservationValue -> Lude.Maybe Lude.Text) (\s a -> s {remainingUpfrontValue = a} :: ReservationValue)
{-# DEPRECATED rvRemainingUpfrontValue "Use generic-lens or generic-optics with 'remainingUpfrontValue' instead." #-}

instance Lude.FromXML ReservationValue where
  parseXML x =
    ReservationValue'
      Lude.<$> (x Lude..@? "hourlyPrice")
      Lude.<*> (x Lude..@? "remainingTotalValue")
      Lude.<*> (x Lude..@? "remainingUpfrontValue")
