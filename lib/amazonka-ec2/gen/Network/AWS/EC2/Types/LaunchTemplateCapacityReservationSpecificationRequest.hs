{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateCapacityReservationSpecificationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateCapacityReservationSpecificationRequest
  ( LaunchTemplateCapacityReservationSpecificationRequest (..),

    -- * Smart constructor
    mkLaunchTemplateCapacityReservationSpecificationRequest,

    -- * Lenses
    ltcrsrCapacityReservationPreference,
    ltcrsrCapacityReservationTarget,
  )
where

import qualified Network.AWS.EC2.Types.CapacityReservationPreference as Types
import qualified Network.AWS.EC2.Types.CapacityReservationTarget as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an instance's Capacity Reservation targeting option. You can specify only one option at a time. Use the @CapacityReservationPreference@ parameter to configure the instance to run in On-Demand capacity or to run in any @open@ Capacity Reservation that has matching attributes (instance type, platform, Availability Zone). Use the @CapacityReservationTarget@ parameter to explicitly target a specific Capacity Reservation or a Capacity Reservation group.
--
-- /See:/ 'mkLaunchTemplateCapacityReservationSpecificationRequest' smart constructor.
data LaunchTemplateCapacityReservationSpecificationRequest = LaunchTemplateCapacityReservationSpecificationRequest'
  { -- | Indicates the instance's Capacity Reservation preferences. Possible preferences include:
    --
    --
    --     * @open@ - The instance can run in any @open@ Capacity Reservation that has matching attributes (instance type, platform, Availability Zone).
    --
    --
    --     * @none@ - The instance avoids running in a Capacity Reservation even if one is available. The instance runs in On-Demand capacity.
    capacityReservationPreference :: Core.Maybe Types.CapacityReservationPreference,
    -- | Information about the target Capacity Reservation or Capacity Reservation group.
    capacityReservationTarget :: Core.Maybe Types.CapacityReservationTarget
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchTemplateCapacityReservationSpecificationRequest' value with any optional fields omitted.
mkLaunchTemplateCapacityReservationSpecificationRequest ::
  LaunchTemplateCapacityReservationSpecificationRequest
mkLaunchTemplateCapacityReservationSpecificationRequest =
  LaunchTemplateCapacityReservationSpecificationRequest'
    { capacityReservationPreference =
        Core.Nothing,
      capacityReservationTarget = Core.Nothing
    }

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
ltcrsrCapacityReservationPreference :: Lens.Lens' LaunchTemplateCapacityReservationSpecificationRequest (Core.Maybe Types.CapacityReservationPreference)
ltcrsrCapacityReservationPreference = Lens.field @"capacityReservationPreference"
{-# DEPRECATED ltcrsrCapacityReservationPreference "Use generic-lens or generic-optics with 'capacityReservationPreference' instead." #-}

-- | Information about the target Capacity Reservation or Capacity Reservation group.
--
-- /Note:/ Consider using 'capacityReservationTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcrsrCapacityReservationTarget :: Lens.Lens' LaunchTemplateCapacityReservationSpecificationRequest (Core.Maybe Types.CapacityReservationTarget)
ltcrsrCapacityReservationTarget = Lens.field @"capacityReservationTarget"
{-# DEPRECATED ltcrsrCapacityReservationTarget "Use generic-lens or generic-optics with 'capacityReservationTarget' instead." #-}
