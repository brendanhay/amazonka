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
    ltcrsrCapacityReservationTarget,
    ltcrsrCapacityReservationPreference,
  )
where

import Network.AWS.EC2.Types.CapacityReservationPreference
import Network.AWS.EC2.Types.CapacityReservationTarget
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an instance's Capacity Reservation targeting option. You can specify only one option at a time. Use the @CapacityReservationPreference@ parameter to configure the instance to run in On-Demand capacity or to run in any @open@ Capacity Reservation that has matching attributes (instance type, platform, Availability Zone). Use the @CapacityReservationTarget@ parameter to explicitly target a specific Capacity Reservation or a Capacity Reservation group.
--
-- /See:/ 'mkLaunchTemplateCapacityReservationSpecificationRequest' smart constructor.
data LaunchTemplateCapacityReservationSpecificationRequest = LaunchTemplateCapacityReservationSpecificationRequest'
  { capacityReservationTarget ::
      Lude.Maybe
        CapacityReservationTarget,
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

-- | Creates a value of 'LaunchTemplateCapacityReservationSpecificationRequest' with the minimum fields required to make a request.
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
mkLaunchTemplateCapacityReservationSpecificationRequest ::
  LaunchTemplateCapacityReservationSpecificationRequest
mkLaunchTemplateCapacityReservationSpecificationRequest =
  LaunchTemplateCapacityReservationSpecificationRequest'
    { capacityReservationTarget =
        Lude.Nothing,
      capacityReservationPreference =
        Lude.Nothing
    }

-- | Information about the target Capacity Reservation or Capacity Reservation group.
--
-- /Note:/ Consider using 'capacityReservationTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltcrsrCapacityReservationTarget :: Lens.Lens' LaunchTemplateCapacityReservationSpecificationRequest (Lude.Maybe CapacityReservationTarget)
ltcrsrCapacityReservationTarget = Lens.lens (capacityReservationTarget :: LaunchTemplateCapacityReservationSpecificationRequest -> Lude.Maybe CapacityReservationTarget) (\s a -> s {capacityReservationTarget = a} :: LaunchTemplateCapacityReservationSpecificationRequest)
{-# DEPRECATED ltcrsrCapacityReservationTarget "Use generic-lens or generic-optics with 'capacityReservationTarget' instead." #-}

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
ltcrsrCapacityReservationPreference :: Lens.Lens' LaunchTemplateCapacityReservationSpecificationRequest (Lude.Maybe CapacityReservationPreference)
ltcrsrCapacityReservationPreference = Lens.lens (capacityReservationPreference :: LaunchTemplateCapacityReservationSpecificationRequest -> Lude.Maybe CapacityReservationPreference) (\s a -> s {capacityReservationPreference = a} :: LaunchTemplateCapacityReservationSpecificationRequest)
{-# DEPRECATED ltcrsrCapacityReservationPreference "Use generic-lens or generic-optics with 'capacityReservationPreference' instead." #-}

instance
  Lude.ToQuery
    LaunchTemplateCapacityReservationSpecificationRequest
  where
  toQuery LaunchTemplateCapacityReservationSpecificationRequest' {..} =
    Lude.mconcat
      [ "CapacityReservationTarget" Lude.=: capacityReservationTarget,
        "CapacityReservationPreference"
          Lude.=: capacityReservationPreference
      ]
