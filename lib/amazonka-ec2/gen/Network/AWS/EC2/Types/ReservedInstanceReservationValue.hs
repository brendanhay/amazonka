{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ReservedInstanceReservationValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ReservedInstanceReservationValue
  ( ReservedInstanceReservationValue (..),

    -- * Smart constructor
    mkReservedInstanceReservationValue,

    -- * Lenses
    rirvReservationValue,
    rirvReservedInstanceId,
  )
where

import Network.AWS.EC2.Types.ReservationValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The total value of the Convertible Reserved Instance.
--
-- /See:/ 'mkReservedInstanceReservationValue' smart constructor.
data ReservedInstanceReservationValue = ReservedInstanceReservationValue'
  { reservationValue ::
      Lude.Maybe
        ReservationValue,
    reservedInstanceId ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReservedInstanceReservationValue' with the minimum fields required to make a request.
--
-- * 'reservationValue' - The total value of the Convertible Reserved Instance that you are exchanging.
-- * 'reservedInstanceId' - The ID of the Convertible Reserved Instance that you are exchanging.
mkReservedInstanceReservationValue ::
  ReservedInstanceReservationValue
mkReservedInstanceReservationValue =
  ReservedInstanceReservationValue'
    { reservationValue =
        Lude.Nothing,
      reservedInstanceId = Lude.Nothing
    }

-- | The total value of the Convertible Reserved Instance that you are exchanging.
--
-- /Note:/ Consider using 'reservationValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rirvReservationValue :: Lens.Lens' ReservedInstanceReservationValue (Lude.Maybe ReservationValue)
rirvReservationValue = Lens.lens (reservationValue :: ReservedInstanceReservationValue -> Lude.Maybe ReservationValue) (\s a -> s {reservationValue = a} :: ReservedInstanceReservationValue)
{-# DEPRECATED rirvReservationValue "Use generic-lens or generic-optics with 'reservationValue' instead." #-}

-- | The ID of the Convertible Reserved Instance that you are exchanging.
--
-- /Note:/ Consider using 'reservedInstanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rirvReservedInstanceId :: Lens.Lens' ReservedInstanceReservationValue (Lude.Maybe Lude.Text)
rirvReservedInstanceId = Lens.lens (reservedInstanceId :: ReservedInstanceReservationValue -> Lude.Maybe Lude.Text) (\s a -> s {reservedInstanceId = a} :: ReservedInstanceReservationValue)
{-# DEPRECATED rirvReservedInstanceId "Use generic-lens or generic-optics with 'reservedInstanceId' instead." #-}

instance Lude.FromXML ReservedInstanceReservationValue where
  parseXML x =
    ReservedInstanceReservationValue'
      Lude.<$> (x Lude..@? "reservationValue")
      Lude.<*> (x Lude..@? "reservedInstanceId")
