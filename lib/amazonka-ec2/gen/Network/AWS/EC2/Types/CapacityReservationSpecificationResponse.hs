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
    crsCapacityReservationTarget,
    crsCapacityReservationPreference,
  )
where

import Network.AWS.EC2.Types.CapacityReservationPreference
import Network.AWS.EC2.Types.CapacityReservationTargetResponse
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the instance's Capacity Reservation targeting preferences. The action returns the @capacityReservationPreference@ response element if the instance is configured to run in On-Demand capacity, or if it is configured in run in any @open@ Capacity Reservation that has matching attributes (instance type, platform, Availability Zone). The action returns the @capacityReservationTarget@ response element if the instance explicily targets a specific Capacity Reservation or Capacity Reservation group.
--
-- /See:/ 'mkCapacityReservationSpecificationResponse' smart constructor.
data CapacityReservationSpecificationResponse = CapacityReservationSpecificationResponse'
  { capacityReservationTarget ::
      Lude.Maybe
        CapacityReservationTargetResponse,
    capacityReservationPreference ::
      Lude.Maybe
        CapacityReservationPreference
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CapacityReservationSpecificationResponse' with the minimum fields required to make a request.
--
-- * 'capacityReservationPreference' - Describes the instance's Capacity Reservation preferences. Possible preferences include:
--
--
--     * @open@ - The instance can run in any @open@ Capacity Reservation that has matching attributes (instance type, platform, Availability Zone).
--
--
--     * @none@ - The instance avoids running in a Capacity Reservation even if one is available. The instance runs in On-Demand capacity.
--
--
-- * 'capacityReservationTarget' - Information about the targeted Capacity Reservation or Capacity Reservation group.
mkCapacityReservationSpecificationResponse ::
  CapacityReservationSpecificationResponse
mkCapacityReservationSpecificationResponse =
  CapacityReservationSpecificationResponse'
    { capacityReservationTarget =
        Lude.Nothing,
      capacityReservationPreference = Lude.Nothing
    }

-- | Information about the targeted Capacity Reservation or Capacity Reservation group.
--
-- /Note:/ Consider using 'capacityReservationTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crsCapacityReservationTarget :: Lens.Lens' CapacityReservationSpecificationResponse (Lude.Maybe CapacityReservationTargetResponse)
crsCapacityReservationTarget = Lens.lens (capacityReservationTarget :: CapacityReservationSpecificationResponse -> Lude.Maybe CapacityReservationTargetResponse) (\s a -> s {capacityReservationTarget = a} :: CapacityReservationSpecificationResponse)
{-# DEPRECATED crsCapacityReservationTarget "Use generic-lens or generic-optics with 'capacityReservationTarget' instead." #-}

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
crsCapacityReservationPreference :: Lens.Lens' CapacityReservationSpecificationResponse (Lude.Maybe CapacityReservationPreference)
crsCapacityReservationPreference = Lens.lens (capacityReservationPreference :: CapacityReservationSpecificationResponse -> Lude.Maybe CapacityReservationPreference) (\s a -> s {capacityReservationPreference = a} :: CapacityReservationSpecificationResponse)
{-# DEPRECATED crsCapacityReservationPreference "Use generic-lens or generic-optics with 'capacityReservationPreference' instead." #-}

instance Lude.FromXML CapacityReservationSpecificationResponse where
  parseXML x =
    CapacityReservationSpecificationResponse'
      Lude.<$> (x Lude..@? "capacityReservationTarget")
      Lude.<*> (x Lude..@? "capacityReservationPreference")
