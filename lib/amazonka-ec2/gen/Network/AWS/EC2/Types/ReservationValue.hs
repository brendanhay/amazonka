{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ReservationValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.ReservationValue
  ( ReservationValue (..)
  -- * Smart constructor
  , mkReservationValue
  -- * Lenses
  , rvHourlyPrice
  , rvRemainingTotalValue
  , rvRemainingUpfrontValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The cost associated with the Reserved Instance.
--
-- /See:/ 'mkReservationValue' smart constructor.
data ReservationValue = ReservationValue'
  { hourlyPrice :: Core.Maybe Core.Text
    -- ^ The hourly rate of the reservation.
  , remainingTotalValue :: Core.Maybe Core.Text
    -- ^ The balance of the total value (the sum of remainingUpfrontValue + hourlyPrice * number of hours remaining).
  , remainingUpfrontValue :: Core.Maybe Core.Text
    -- ^ The remaining upfront cost of the reservation.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReservationValue' value with any optional fields omitted.
mkReservationValue
    :: ReservationValue
mkReservationValue
  = ReservationValue'{hourlyPrice = Core.Nothing,
                      remainingTotalValue = Core.Nothing,
                      remainingUpfrontValue = Core.Nothing}

-- | The hourly rate of the reservation.
--
-- /Note:/ Consider using 'hourlyPrice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvHourlyPrice :: Lens.Lens' ReservationValue (Core.Maybe Core.Text)
rvHourlyPrice = Lens.field @"hourlyPrice"
{-# INLINEABLE rvHourlyPrice #-}
{-# DEPRECATED hourlyPrice "Use generic-lens or generic-optics with 'hourlyPrice' instead"  #-}

-- | The balance of the total value (the sum of remainingUpfrontValue + hourlyPrice * number of hours remaining).
--
-- /Note:/ Consider using 'remainingTotalValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvRemainingTotalValue :: Lens.Lens' ReservationValue (Core.Maybe Core.Text)
rvRemainingTotalValue = Lens.field @"remainingTotalValue"
{-# INLINEABLE rvRemainingTotalValue #-}
{-# DEPRECATED remainingTotalValue "Use generic-lens or generic-optics with 'remainingTotalValue' instead"  #-}

-- | The remaining upfront cost of the reservation.
--
-- /Note:/ Consider using 'remainingUpfrontValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rvRemainingUpfrontValue :: Lens.Lens' ReservationValue (Core.Maybe Core.Text)
rvRemainingUpfrontValue = Lens.field @"remainingUpfrontValue"
{-# INLINEABLE rvRemainingUpfrontValue #-}
{-# DEPRECATED remainingUpfrontValue "Use generic-lens or generic-optics with 'remainingUpfrontValue' instead"  #-}

instance Core.FromXML ReservationValue where
        parseXML x
          = ReservationValue' Core.<$>
              (x Core..@? "hourlyPrice") Core.<*>
                x Core..@? "remainingTotalValue"
                Core.<*> x Core..@? "remainingUpfrontValue"
