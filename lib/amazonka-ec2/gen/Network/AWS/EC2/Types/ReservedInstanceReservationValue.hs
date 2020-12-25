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

import qualified Network.AWS.EC2.Types.ReservationValue as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The total value of the Convertible Reserved Instance.
--
-- /See:/ 'mkReservedInstanceReservationValue' smart constructor.
data ReservedInstanceReservationValue = ReservedInstanceReservationValue'
  { -- | The total value of the Convertible Reserved Instance that you are exchanging.
    reservationValue :: Core.Maybe Types.ReservationValue,
    -- | The ID of the Convertible Reserved Instance that you are exchanging.
    reservedInstanceId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReservedInstanceReservationValue' value with any optional fields omitted.
mkReservedInstanceReservationValue ::
  ReservedInstanceReservationValue
mkReservedInstanceReservationValue =
  ReservedInstanceReservationValue'
    { reservationValue =
        Core.Nothing,
      reservedInstanceId = Core.Nothing
    }

-- | The total value of the Convertible Reserved Instance that you are exchanging.
--
-- /Note:/ Consider using 'reservationValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rirvReservationValue :: Lens.Lens' ReservedInstanceReservationValue (Core.Maybe Types.ReservationValue)
rirvReservationValue = Lens.field @"reservationValue"
{-# DEPRECATED rirvReservationValue "Use generic-lens or generic-optics with 'reservationValue' instead." #-}

-- | The ID of the Convertible Reserved Instance that you are exchanging.
--
-- /Note:/ Consider using 'reservedInstanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rirvReservedInstanceId :: Lens.Lens' ReservedInstanceReservationValue (Core.Maybe Types.String)
rirvReservedInstanceId = Lens.field @"reservedInstanceId"
{-# DEPRECATED rirvReservedInstanceId "Use generic-lens or generic-optics with 'reservedInstanceId' instead." #-}

instance Core.FromXML ReservedInstanceReservationValue where
  parseXML x =
    ReservedInstanceReservationValue'
      Core.<$> (x Core..@? "reservationValue")
      Core.<*> (x Core..@? "reservedInstanceId")
