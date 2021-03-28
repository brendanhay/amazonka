{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CapacityReservationSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.CapacityReservationSpecification
  ( CapacityReservationSpecification (..)
  -- * Smart constructor
  , mkCapacityReservationSpecification
  -- * Lenses
  , crsCapacityReservationPreference
  , crsCapacityReservationTarget
  ) where

import qualified Network.AWS.EC2.Types.CapacityReservationPreference as Types
import qualified Network.AWS.EC2.Types.CapacityReservationTarget as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an instance's Capacity Reservation targeting option. You can specify only one parameter at a time. If you specify @CapacityReservationPreference@ and @CapacityReservationTarget@ , the request fails.
--
-- Use the @CapacityReservationPreference@ parameter to configure the instance to run as an On-Demand Instance or to run in any @open@ Capacity Reservation that has matching attributes (instance type, platform, Availability Zone). Use the @CapacityReservationTarget@ parameter to explicitly target a specific Capacity Reservation or a Capacity Reservation group.
--
-- /See:/ 'mkCapacityReservationSpecification' smart constructor.
data CapacityReservationSpecification = CapacityReservationSpecification'
  { capacityReservationPreference :: Core.Maybe Types.CapacityReservationPreference
    -- ^ Indicates the instance's Capacity Reservation preferences. Possible preferences include:
--
--
--     * @open@ - The instance can run in any @open@ Capacity Reservation that has matching attributes (instance type, platform, Availability Zone).
--
--
--     * @none@ - The instance avoids running in a Capacity Reservation even if one is available. The instance runs as an On-Demand Instance.
--
--
  , capacityReservationTarget :: Core.Maybe Types.CapacityReservationTarget
    -- ^ Information about the target Capacity Reservation or Capacity Reservation group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CapacityReservationSpecification' value with any optional fields omitted.
mkCapacityReservationSpecification
    :: CapacityReservationSpecification
mkCapacityReservationSpecification
  = CapacityReservationSpecification'{capacityReservationPreference =
                                        Core.Nothing,
                                      capacityReservationTarget = Core.Nothing}

-- | Indicates the instance's Capacity Reservation preferences. Possible preferences include:
--
--
--     * @open@ - The instance can run in any @open@ Capacity Reservation that has matching attributes (instance type, platform, Availability Zone).
--
--
--     * @none@ - The instance avoids running in a Capacity Reservation even if one is available. The instance runs as an On-Demand Instance.
--
--
--
-- /Note:/ Consider using 'capacityReservationPreference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsCapacityReservationPreference :: Lens.Lens' CapacityReservationSpecification (Core.Maybe Types.CapacityReservationPreference)
crsCapacityReservationPreference = Lens.field @"capacityReservationPreference"
{-# INLINEABLE crsCapacityReservationPreference #-}
{-# DEPRECATED capacityReservationPreference "Use generic-lens or generic-optics with 'capacityReservationPreference' instead"  #-}

-- | Information about the target Capacity Reservation or Capacity Reservation group.
--
-- /Note:/ Consider using 'capacityReservationTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsCapacityReservationTarget :: Lens.Lens' CapacityReservationSpecification (Core.Maybe Types.CapacityReservationTarget)
crsCapacityReservationTarget = Lens.field @"capacityReservationTarget"
{-# INLINEABLE crsCapacityReservationTarget #-}
{-# DEPRECATED capacityReservationTarget "Use generic-lens or generic-optics with 'capacityReservationTarget' instead"  #-}

instance Core.ToQuery CapacityReservationSpecification where
        toQuery CapacityReservationSpecification{..}
          = Core.maybe Core.mempty
              (Core.toQueryPair "CapacityReservationPreference")
              capacityReservationPreference
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "CapacityReservationTarget")
                capacityReservationTarget
