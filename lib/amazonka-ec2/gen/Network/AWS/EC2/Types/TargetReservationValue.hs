{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TargetReservationValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.TargetReservationValue
  ( TargetReservationValue (..),

    -- * Smart constructor
    mkTargetReservationValue,

    -- * Lenses
    trvReservationValue,
    trvTargetConfiguration,
  )
where

import Network.AWS.EC2.Types.ReservationValue
import Network.AWS.EC2.Types.TargetConfiguration
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The total value of the new Convertible Reserved Instances.
--
-- /See:/ 'mkTargetReservationValue' smart constructor.
data TargetReservationValue = TargetReservationValue'
  { reservationValue ::
      Lude.Maybe ReservationValue,
    targetConfiguration ::
      Lude.Maybe TargetConfiguration
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TargetReservationValue' with the minimum fields required to make a request.
--
-- * 'reservationValue' - The total value of the Convertible Reserved Instances that make up the exchange. This is the sum of the list value, remaining upfront price, and additional upfront cost of the exchange.
-- * 'targetConfiguration' - The configuration of the Convertible Reserved Instances that make up the exchange.
mkTargetReservationValue ::
  TargetReservationValue
mkTargetReservationValue =
  TargetReservationValue'
    { reservationValue = Lude.Nothing,
      targetConfiguration = Lude.Nothing
    }

-- | The total value of the Convertible Reserved Instances that make up the exchange. This is the sum of the list value, remaining upfront price, and additional upfront cost of the exchange.
--
-- /Note:/ Consider using 'reservationValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trvReservationValue :: Lens.Lens' TargetReservationValue (Lude.Maybe ReservationValue)
trvReservationValue = Lens.lens (reservationValue :: TargetReservationValue -> Lude.Maybe ReservationValue) (\s a -> s {reservationValue = a} :: TargetReservationValue)
{-# DEPRECATED trvReservationValue "Use generic-lens or generic-optics with 'reservationValue' instead." #-}

-- | The configuration of the Convertible Reserved Instances that make up the exchange.
--
-- /Note:/ Consider using 'targetConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trvTargetConfiguration :: Lens.Lens' TargetReservationValue (Lude.Maybe TargetConfiguration)
trvTargetConfiguration = Lens.lens (targetConfiguration :: TargetReservationValue -> Lude.Maybe TargetConfiguration) (\s a -> s {targetConfiguration = a} :: TargetReservationValue)
{-# DEPRECATED trvTargetConfiguration "Use generic-lens or generic-optics with 'targetConfiguration' instead." #-}

instance Lude.FromXML TargetReservationValue where
  parseXML x =
    TargetReservationValue'
      Lude.<$> (x Lude..@? "reservationValue")
      Lude.<*> (x Lude..@? "targetConfiguration")
