-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CapacityReservationTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CapacityReservationTarget
  ( CapacityReservationTarget (..),

    -- * Smart constructor
    mkCapacityReservationTarget,

    -- * Lenses
    crtCapacityReservationId,
    crtCapacityReservationResourceGroupARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a target Capacity Reservation or Capacity Reservation group.
--
-- /See:/ 'mkCapacityReservationTarget' smart constructor.
data CapacityReservationTarget = CapacityReservationTarget'
  { capacityReservationId ::
      Lude.Maybe Lude.Text,
    capacityReservationResourceGroupARN ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CapacityReservationTarget' with the minimum fields required to make a request.
--
-- * 'capacityReservationId' - The ID of the Capacity Reservation in which to run the instance.
-- * 'capacityReservationResourceGroupARN' - The ARN of the Capacity Reservation resource group in which to run the instance.
mkCapacityReservationTarget ::
  CapacityReservationTarget
mkCapacityReservationTarget =
  CapacityReservationTarget'
    { capacityReservationId = Lude.Nothing,
      capacityReservationResourceGroupARN = Lude.Nothing
    }

-- | The ID of the Capacity Reservation in which to run the instance.
--
-- /Note:/ Consider using 'capacityReservationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtCapacityReservationId :: Lens.Lens' CapacityReservationTarget (Lude.Maybe Lude.Text)
crtCapacityReservationId = Lens.lens (capacityReservationId :: CapacityReservationTarget -> Lude.Maybe Lude.Text) (\s a -> s {capacityReservationId = a} :: CapacityReservationTarget)
{-# DEPRECATED crtCapacityReservationId "Use generic-lens or generic-optics with 'capacityReservationId' instead." #-}

-- | The ARN of the Capacity Reservation resource group in which to run the instance.
--
-- /Note:/ Consider using 'capacityReservationResourceGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtCapacityReservationResourceGroupARN :: Lens.Lens' CapacityReservationTarget (Lude.Maybe Lude.Text)
crtCapacityReservationResourceGroupARN = Lens.lens (capacityReservationResourceGroupARN :: CapacityReservationTarget -> Lude.Maybe Lude.Text) (\s a -> s {capacityReservationResourceGroupARN = a} :: CapacityReservationTarget)
{-# DEPRECATED crtCapacityReservationResourceGroupARN "Use generic-lens or generic-optics with 'capacityReservationResourceGroupARN' instead." #-}

instance Lude.ToQuery CapacityReservationTarget where
  toQuery CapacityReservationTarget' {..} =
    Lude.mconcat
      [ "CapacityReservationId" Lude.=: capacityReservationId,
        "CapacityReservationResourceGroupArn"
          Lude.=: capacityReservationResourceGroupARN
      ]
