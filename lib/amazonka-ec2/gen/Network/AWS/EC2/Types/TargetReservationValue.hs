{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.TargetReservationValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.TargetReservationValue
  ( TargetReservationValue (..)
  -- * Smart constructor
  , mkTargetReservationValue
  -- * Lenses
  , trvReservationValue
  , trvTargetConfiguration
  ) where

import qualified Network.AWS.EC2.Types.ReservationValue as Types
import qualified Network.AWS.EC2.Types.TargetConfiguration as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The total value of the new Convertible Reserved Instances.
--
-- /See:/ 'mkTargetReservationValue' smart constructor.
data TargetReservationValue = TargetReservationValue'
  { reservationValue :: Core.Maybe Types.ReservationValue
    -- ^ The total value of the Convertible Reserved Instances that make up the exchange. This is the sum of the list value, remaining upfront price, and additional upfront cost of the exchange.
  , targetConfiguration :: Core.Maybe Types.TargetConfiguration
    -- ^ The configuration of the Convertible Reserved Instances that make up the exchange.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TargetReservationValue' value with any optional fields omitted.
mkTargetReservationValue
    :: TargetReservationValue
mkTargetReservationValue
  = TargetReservationValue'{reservationValue = Core.Nothing,
                            targetConfiguration = Core.Nothing}

-- | The total value of the Convertible Reserved Instances that make up the exchange. This is the sum of the list value, remaining upfront price, and additional upfront cost of the exchange.
--
-- /Note:/ Consider using 'reservationValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trvReservationValue :: Lens.Lens' TargetReservationValue (Core.Maybe Types.ReservationValue)
trvReservationValue = Lens.field @"reservationValue"
{-# INLINEABLE trvReservationValue #-}
{-# DEPRECATED reservationValue "Use generic-lens or generic-optics with 'reservationValue' instead"  #-}

-- | The configuration of the Convertible Reserved Instances that make up the exchange.
--
-- /Note:/ Consider using 'targetConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trvTargetConfiguration :: Lens.Lens' TargetReservationValue (Core.Maybe Types.TargetConfiguration)
trvTargetConfiguration = Lens.field @"targetConfiguration"
{-# INLINEABLE trvTargetConfiguration #-}
{-# DEPRECATED targetConfiguration "Use generic-lens or generic-optics with 'targetConfiguration' instead"  #-}

instance Core.FromXML TargetReservationValue where
        parseXML x
          = TargetReservationValue' Core.<$>
              (x Core..@? "reservationValue") Core.<*>
                x Core..@? "targetConfiguration"
