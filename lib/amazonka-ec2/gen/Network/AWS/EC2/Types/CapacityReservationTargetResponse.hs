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
    cCapacityReservationId,
    cCapacityReservationResourceGroupARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a target Capacity Reservation or Capacity Reservation group.
--
-- /See:/ 'mkCapacityReservationTargetResponse' smart constructor.
data CapacityReservationTargetResponse = CapacityReservationTargetResponse'
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

-- | Creates a value of 'CapacityReservationTargetResponse' with the minimum fields required to make a request.
--
-- * 'capacityReservationId' - The ID of the targeted Capacity Reservation.
-- * 'capacityReservationResourceGroupARN' - The ARN of the targeted Capacity Reservation group.
mkCapacityReservationTargetResponse ::
  CapacityReservationTargetResponse
mkCapacityReservationTargetResponse =
  CapacityReservationTargetResponse'
    { capacityReservationId =
        Lude.Nothing,
      capacityReservationResourceGroupARN = Lude.Nothing
    }

-- | The ID of the targeted Capacity Reservation.
--
-- /Note:/ Consider using 'capacityReservationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCapacityReservationId :: Lens.Lens' CapacityReservationTargetResponse (Lude.Maybe Lude.Text)
cCapacityReservationId = Lens.lens (capacityReservationId :: CapacityReservationTargetResponse -> Lude.Maybe Lude.Text) (\s a -> s {capacityReservationId = a} :: CapacityReservationTargetResponse)
{-# DEPRECATED cCapacityReservationId "Use generic-lens or generic-optics with 'capacityReservationId' instead." #-}

-- | The ARN of the targeted Capacity Reservation group.
--
-- /Note:/ Consider using 'capacityReservationResourceGroupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCapacityReservationResourceGroupARN :: Lens.Lens' CapacityReservationTargetResponse (Lude.Maybe Lude.Text)
cCapacityReservationResourceGroupARN = Lens.lens (capacityReservationResourceGroupARN :: CapacityReservationTargetResponse -> Lude.Maybe Lude.Text) (\s a -> s {capacityReservationResourceGroupARN = a} :: CapacityReservationTargetResponse)
{-# DEPRECATED cCapacityReservationResourceGroupARN "Use generic-lens or generic-optics with 'capacityReservationResourceGroupARN' instead." #-}

instance Lude.FromXML CapacityReservationTargetResponse where
  parseXML x =
    CapacityReservationTargetResponse'
      Lude.<$> (x Lude..@? "capacityReservationId")
      Lude.<*> (x Lude..@? "capacityReservationResourceGroupArn")
