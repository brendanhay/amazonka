{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CapacityReservationSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CapacityReservationSpecification
  ( CapacityReservationSpecification (..),

    -- * Smart constructor
    mkCapacityReservationSpecification,

    -- * Lenses
    cCapacityReservationTarget,
    cCapacityReservationPreference,
  )
where

import Network.AWS.EC2.Types.CapacityReservationPreference
import Network.AWS.EC2.Types.CapacityReservationTarget
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an instance's Capacity Reservation targeting option. You can specify only one parameter at a time. If you specify @CapacityReservationPreference@ and @CapacityReservationTarget@ , the request fails.
--
-- Use the @CapacityReservationPreference@ parameter to configure the instance to run as an On-Demand Instance or to run in any @open@ Capacity Reservation that has matching attributes (instance type, platform, Availability Zone). Use the @CapacityReservationTarget@ parameter to explicitly target a specific Capacity Reservation or a Capacity Reservation group.
--
-- /See:/ 'mkCapacityReservationSpecification' smart constructor.
data CapacityReservationSpecification = CapacityReservationSpecification'
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
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CapacityReservationSpecification' with the minimum fields required to make a request.
--
-- * 'capacityReservationPreference' - Indicates the instance's Capacity Reservation preferences. Possible preferences include:
--
--
--     * @open@ - The instance can run in any @open@ Capacity Reservation that has matching attributes (instance type, platform, Availability Zone).
--
--
--     * @none@ - The instance avoids running in a Capacity Reservation even if one is available. The instance runs as an On-Demand Instance.
--
--
-- * 'capacityReservationTarget' - Information about the target Capacity Reservation or Capacity Reservation group.
mkCapacityReservationSpecification ::
  CapacityReservationSpecification
mkCapacityReservationSpecification =
  CapacityReservationSpecification'
    { capacityReservationTarget =
        Lude.Nothing,
      capacityReservationPreference = Lude.Nothing
    }

-- | Information about the target Capacity Reservation or Capacity Reservation group.
--
-- /Note:/ Consider using 'capacityReservationTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCapacityReservationTarget :: Lens.Lens' CapacityReservationSpecification (Lude.Maybe CapacityReservationTarget)
cCapacityReservationTarget = Lens.lens (capacityReservationTarget :: CapacityReservationSpecification -> Lude.Maybe CapacityReservationTarget) (\s a -> s {capacityReservationTarget = a} :: CapacityReservationSpecification)
{-# DEPRECATED cCapacityReservationTarget "Use generic-lens or generic-optics with 'capacityReservationTarget' instead." #-}

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
cCapacityReservationPreference :: Lens.Lens' CapacityReservationSpecification (Lude.Maybe CapacityReservationPreference)
cCapacityReservationPreference = Lens.lens (capacityReservationPreference :: CapacityReservationSpecification -> Lude.Maybe CapacityReservationPreference) (\s a -> s {capacityReservationPreference = a} :: CapacityReservationSpecification)
{-# DEPRECATED cCapacityReservationPreference "Use generic-lens or generic-optics with 'capacityReservationPreference' instead." #-}

instance Lude.ToQuery CapacityReservationSpecification where
  toQuery CapacityReservationSpecification' {..} =
    Lude.mconcat
      [ "CapacityReservationTarget" Lude.=: capacityReservationTarget,
        "CapacityReservationPreference"
          Lude.=: capacityReservationPreference
      ]
