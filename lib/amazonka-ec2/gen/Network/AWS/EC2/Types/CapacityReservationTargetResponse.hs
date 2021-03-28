{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CapacityReservationTargetResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.CapacityReservationTargetResponse
  ( CapacityReservationTargetResponse (..)
  -- * Smart constructor
  , mkCapacityReservationTargetResponse
  -- * Lenses
  , crtrCapacityReservationId
  , crtrCapacityReservationResourceGroupArn
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a target Capacity Reservation or Capacity Reservation group.
--
-- /See:/ 'mkCapacityReservationTargetResponse' smart constructor.
data CapacityReservationTargetResponse = CapacityReservationTargetResponse'
  { capacityReservationId :: Core.Maybe Core.Text
    -- ^ The ID of the targeted Capacity Reservation.
  , capacityReservationResourceGroupArn :: Core.Maybe Core.Text
    -- ^ The ARN of the targeted Capacity Reservation group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CapacityReservationTargetResponse' value with any optional fields omitted.
mkCapacityReservationTargetResponse
    :: CapacityReservationTargetResponse
mkCapacityReservationTargetResponse
  = CapacityReservationTargetResponse'{capacityReservationId =
                                         Core.Nothing,
                                       capacityReservationResourceGroupArn = Core.Nothing}

-- | The ID of the targeted Capacity Reservation.
--
-- /Note:/ Consider using 'capacityReservationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtrCapacityReservationId :: Lens.Lens' CapacityReservationTargetResponse (Core.Maybe Core.Text)
crtrCapacityReservationId = Lens.field @"capacityReservationId"
{-# INLINEABLE crtrCapacityReservationId #-}
{-# DEPRECATED capacityReservationId "Use generic-lens or generic-optics with 'capacityReservationId' instead"  #-}

-- | The ARN of the targeted Capacity Reservation group.
--
-- /Note:/ Consider using 'capacityReservationResourceGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtrCapacityReservationResourceGroupArn :: Lens.Lens' CapacityReservationTargetResponse (Core.Maybe Core.Text)
crtrCapacityReservationResourceGroupArn = Lens.field @"capacityReservationResourceGroupArn"
{-# INLINEABLE crtrCapacityReservationResourceGroupArn #-}
{-# DEPRECATED capacityReservationResourceGroupArn "Use generic-lens or generic-optics with 'capacityReservationResourceGroupArn' instead"  #-}

instance Core.FromXML CapacityReservationTargetResponse where
        parseXML x
          = CapacityReservationTargetResponse' Core.<$>
              (x Core..@? "capacityReservationId") Core.<*>
                x Core..@? "capacityReservationResourceGroupArn"
