{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateCapacityReservationSpecificationResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.LaunchTemplateCapacityReservationSpecificationResponse
  ( LaunchTemplateCapacityReservationSpecificationResponse (..)
  -- * Smart constructor
  , mkLaunchTemplateCapacityReservationSpecificationResponse
  -- * Lenses
  , lCapacityReservationPreference
  , lCapacityReservationTarget
  ) where

import qualified Network.AWS.EC2.Types.CapacityReservationPreference as Types
import qualified Network.AWS.EC2.Types.CapacityReservationTargetResponse as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the Capacity Reservation targeting option.
--
-- /See:/ 'mkLaunchTemplateCapacityReservationSpecificationResponse' smart constructor.
data LaunchTemplateCapacityReservationSpecificationResponse = LaunchTemplateCapacityReservationSpecificationResponse'
  { capacityReservationPreference :: Core.Maybe Types.CapacityReservationPreference
    -- ^ Indicates the instance's Capacity Reservation preferences. Possible preferences include:
--
--
--     * @open@ - The instance can run in any @open@ Capacity Reservation that has matching attributes (instance type, platform, Availability Zone).
--
--
--     * @none@ - The instance avoids running in a Capacity Reservation even if one is available. The instance runs in On-Demand capacity.
--
--
  , capacityReservationTarget :: Core.Maybe Types.CapacityReservationTargetResponse
    -- ^ Information about the target Capacity Reservation or Capacity Reservation group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchTemplateCapacityReservationSpecificationResponse' value with any optional fields omitted.
mkLaunchTemplateCapacityReservationSpecificationResponse
    :: LaunchTemplateCapacityReservationSpecificationResponse
mkLaunchTemplateCapacityReservationSpecificationResponse
  = LaunchTemplateCapacityReservationSpecificationResponse'{capacityReservationPreference
                                                              = Core.Nothing,
                                                            capacityReservationTarget =
                                                              Core.Nothing}

-- | Indicates the instance's Capacity Reservation preferences. Possible preferences include:
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
lCapacityReservationPreference :: Lens.Lens' LaunchTemplateCapacityReservationSpecificationResponse (Core.Maybe Types.CapacityReservationPreference)
lCapacityReservationPreference = Lens.field @"capacityReservationPreference"
{-# INLINEABLE lCapacityReservationPreference #-}
{-# DEPRECATED capacityReservationPreference "Use generic-lens or generic-optics with 'capacityReservationPreference' instead"  #-}

-- | Information about the target Capacity Reservation or Capacity Reservation group.
--
-- /Note:/ Consider using 'capacityReservationTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lCapacityReservationTarget :: Lens.Lens' LaunchTemplateCapacityReservationSpecificationResponse (Core.Maybe Types.CapacityReservationTargetResponse)
lCapacityReservationTarget = Lens.field @"capacityReservationTarget"
{-# INLINEABLE lCapacityReservationTarget #-}
{-# DEPRECATED capacityReservationTarget "Use generic-lens or generic-optics with 'capacityReservationTarget' instead"  #-}

instance Core.FromXML
           LaunchTemplateCapacityReservationSpecificationResponse
         where
        parseXML x
          = LaunchTemplateCapacityReservationSpecificationResponse' Core.<$>
              (x Core..@? "capacityReservationPreference") Core.<*>
                x Core..@? "capacityReservationTarget"
