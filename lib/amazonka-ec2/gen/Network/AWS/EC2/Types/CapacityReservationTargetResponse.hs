{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CapacityReservationTargetResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CapacityReservationTargetResponse
  ( CapacityReservationTargetResponse (..),

    -- * Smart constructor
    mkCapacityReservationTargetResponse,

    -- * Lenses
    crtrCapacityReservationId,
    crtrCapacityReservationResourceGroupArn,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a target Capacity Reservation or Capacity Reservation group.
--
-- /See:/ 'mkCapacityReservationTargetResponse' smart constructor.
data CapacityReservationTargetResponse = CapacityReservationTargetResponse'
  { -- | The ID of the targeted Capacity Reservation.
    capacityReservationId :: Core.Maybe Types.String,
    -- | The ARN of the targeted Capacity Reservation group.
    capacityReservationResourceGroupArn :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CapacityReservationTargetResponse' value with any optional fields omitted.
mkCapacityReservationTargetResponse ::
  CapacityReservationTargetResponse
mkCapacityReservationTargetResponse =
  CapacityReservationTargetResponse'
    { capacityReservationId =
        Core.Nothing,
      capacityReservationResourceGroupArn = Core.Nothing
    }

-- | The ID of the targeted Capacity Reservation.
--
-- /Note:/ Consider using 'capacityReservationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtrCapacityReservationId :: Lens.Lens' CapacityReservationTargetResponse (Core.Maybe Types.String)
crtrCapacityReservationId = Lens.field @"capacityReservationId"
{-# DEPRECATED crtrCapacityReservationId "Use generic-lens or generic-optics with 'capacityReservationId' instead." #-}

-- | The ARN of the targeted Capacity Reservation group.
--
-- /Note:/ Consider using 'capacityReservationResourceGroupArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtrCapacityReservationResourceGroupArn :: Lens.Lens' CapacityReservationTargetResponse (Core.Maybe Types.String)
crtrCapacityReservationResourceGroupArn = Lens.field @"capacityReservationResourceGroupArn"
{-# DEPRECATED crtrCapacityReservationResourceGroupArn "Use generic-lens or generic-optics with 'capacityReservationResourceGroupArn' instead." #-}

instance Core.FromXML CapacityReservationTargetResponse where
  parseXML x =
    CapacityReservationTargetResponse'
      Core.<$> (x Core..@? "capacityReservationId")
      Core.<*> (x Core..@? "capacityReservationResourceGroupArn")
