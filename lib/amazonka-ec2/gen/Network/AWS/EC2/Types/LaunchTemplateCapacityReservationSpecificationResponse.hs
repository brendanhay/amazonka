{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateCapacityReservationSpecificationResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateCapacityReservationSpecificationResponse
  ( LaunchTemplateCapacityReservationSpecificationResponse (..),

    -- * Smart constructor
    mkLaunchTemplateCapacityReservationSpecificationResponse,

    -- * Lenses
    ltcrsCapacityReservationTarget,
    ltcrsCapacityReservationPreference,
  )
where

import Network.AWS.EC2.Types.CapacityReservationPreference
import Network.AWS.EC2.Types.CapacityReservationTargetResponse
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the Capacity Reservation targeting option.
--
-- /See:/ 'mkLaunchTemplateCapacityReservationSpecificationResponse' smart constructor.
data LaunchTemplateCapacityReservationSpecificationResponse = LaunchTemplateCapacityReservationSpecificationResponse'
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
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'LaunchTemplateCapacityReservationSpecificationResponse' with the minimum fields required to make a request.
--
-- * 'capacityReservationPreference' - Indicates the instance's Capacity Reservation preferences. Possible preferences include:
--
--
--     * @open@ - The instance can run in any @open@ Capacity Reservation that has matching attributes (instance type, platform, Availability Zone).
--
--
--     * @none@ - The instance avoids running in a Capacity Reservation even if one is available. The instance runs in On-Demand capacity.
--
--
-- * 'capacityReservationTarget' - Information about the target Capacity Reservation or Capacity Reservation group.
mkLaunchTemplateCapacityReservationSpecificationResponse ::
  LaunchTemplateCapacityReservationSpecificationResponse
mkLaunchTemplateCapacityReservationSpecificationResponse =
  LaunchTemplateCapacityReservationSpecificationResponse'
    { capacityReservationTarget =
        Lude.Nothing,
      capacityReservationPreference =
        Lude.Nothing
    }

-- | Information about the target Capacity Reservation or Capacity Reservation group.
--
-- /Note:/ Consider using 'capacityReservationTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcrsCapacityReservationTarget :: Lens.Lens' LaunchTemplateCapacityReservationSpecificationResponse (Lude.Maybe CapacityReservationTargetResponse)
ltcrsCapacityReservationTarget = Lens.lens (capacityReservationTarget :: LaunchTemplateCapacityReservationSpecificationResponse -> Lude.Maybe CapacityReservationTargetResponse) (\s a -> s {capacityReservationTarget = a} :: LaunchTemplateCapacityReservationSpecificationResponse)
{-# DEPRECATED ltcrsCapacityReservationTarget "Use generic-lens or generic-optics with 'capacityReservationTarget' instead." #-}

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
ltcrsCapacityReservationPreference :: Lens.Lens' LaunchTemplateCapacityReservationSpecificationResponse (Lude.Maybe CapacityReservationPreference)
ltcrsCapacityReservationPreference = Lens.lens (capacityReservationPreference :: LaunchTemplateCapacityReservationSpecificationResponse -> Lude.Maybe CapacityReservationPreference) (\s a -> s {capacityReservationPreference = a} :: LaunchTemplateCapacityReservationSpecificationResponse)
{-# DEPRECATED ltcrsCapacityReservationPreference "Use generic-lens or generic-optics with 'capacityReservationPreference' instead." #-}

instance
  Lude.FromXML
    LaunchTemplateCapacityReservationSpecificationResponse
  where
  parseXML x =
    LaunchTemplateCapacityReservationSpecificationResponse'
      Lude.<$> (x Lude..@? "capacityReservationTarget")
      Lude.<*> (x Lude..@? "capacityReservationPreference")
