{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CapacityReservationSpecificationResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CapacityReservationSpecificationResponse
  ( CapacityReservationSpecificationResponse (..),

    -- * Smart constructor
    mkCapacityReservationSpecificationResponse,

    -- * Lenses
    crsrCapacityReservationPreference,
    crsrCapacityReservationTarget,
  )
where

import qualified Network.AWS.EC2.Types.CapacityReservationPreference as Types
import qualified Network.AWS.EC2.Types.CapacityReservationTargetResponse as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the instance's Capacity Reservation targeting preferences. The action returns the @capacityReservationPreference@ response element if the instance is configured to run in On-Demand capacity, or if it is configured in run in any @open@ Capacity Reservation that has matching attributes (instance type, platform, Availability Zone). The action returns the @capacityReservationTarget@ response element if the instance explicily targets a specific Capacity Reservation or Capacity Reservation group.
--
-- /See:/ 'mkCapacityReservationSpecificationResponse' smart constructor.
data CapacityReservationSpecificationResponse = CapacityReservationSpecificationResponse'
  { -- | Describes the instance's Capacity Reservation preferences. Possible preferences include:
    --
    --
    --     * @open@ - The instance can run in any @open@ Capacity Reservation that has matching attributes (instance type, platform, Availability Zone).
    --
    --
    --     * @none@ - The instance avoids running in a Capacity Reservation even if one is available. The instance runs in On-Demand capacity.
    capacityReservationPreference :: Core.Maybe Types.CapacityReservationPreference,
    -- | Information about the targeted Capacity Reservation or Capacity Reservation group.
    capacityReservationTarget :: Core.Maybe Types.CapacityReservationTargetResponse
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CapacityReservationSpecificationResponse' value with any optional fields omitted.
mkCapacityReservationSpecificationResponse ::
  CapacityReservationSpecificationResponse
mkCapacityReservationSpecificationResponse =
  CapacityReservationSpecificationResponse'
    { capacityReservationPreference =
        Core.Nothing,
      capacityReservationTarget = Core.Nothing
    }

-- | Describes the instance's Capacity Reservation preferences. Possible preferences include:
--
--
--     * @open@ - The instance can run in any @open@ Capacity Reservation that has matching attributes (instance type, platform, Availability Zone).
--
--
--     * @none@ - The instance avoids running in a Capacity Reservation even if one is available. The instance runs in On-Demand capacity.
--
--
--
-- /Note:/ Consider using 'capacityReservationPreference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsrCapacityReservationPreference :: Lens.Lens' CapacityReservationSpecificationResponse (Core.Maybe Types.CapacityReservationPreference)
crsrCapacityReservationPreference = Lens.field @"capacityReservationPreference"
{-# DEPRECATED crsrCapacityReservationPreference "Use generic-lens or generic-optics with 'capacityReservationPreference' instead." #-}

-- | Information about the targeted Capacity Reservation or Capacity Reservation group.
--
-- /Note:/ Consider using 'capacityReservationTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsrCapacityReservationTarget :: Lens.Lens' CapacityReservationSpecificationResponse (Core.Maybe Types.CapacityReservationTargetResponse)
crsrCapacityReservationTarget = Lens.field @"capacityReservationTarget"
{-# DEPRECATED crsrCapacityReservationTarget "Use generic-lens or generic-optics with 'capacityReservationTarget' instead." #-}

instance Core.FromXML CapacityReservationSpecificationResponse where
  parseXML x =
    CapacityReservationSpecificationResponse'
      Core.<$> (x Core..@? "capacityReservationPreference")
      Core.<*> (x Core..@? "capacityReservationTarget")
