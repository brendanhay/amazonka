-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Reservation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Reservation
  ( Reservation (..),

    -- * Smart constructor
    mkReservation,

    -- * Lenses
    rGroups,
    rInstances,
    rRequesterId,
    rReservationId,
    rOwnerId,
  )
where

import Network.AWS.EC2.Types.GroupIdentifier
import Network.AWS.EC2.Types.Instance
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a launch request for one or more instances, and includes owner, requester, and security group information that applies to all instances in the launch request.
--
-- /See:/ 'mkReservation' smart constructor.
data Reservation = Reservation'
  { groups ::
      Lude.Maybe [GroupIdentifier],
    instances :: Lude.Maybe [Instance],
    requesterId :: Lude.Maybe Lude.Text,
    reservationId :: Lude.Text,
    ownerId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Reservation' with the minimum fields required to make a request.
--
-- * 'groups' - [EC2-Classic only] The security groups.
-- * 'instances' - The instances.
-- * 'ownerId' - The ID of the AWS account that owns the reservation.
-- * 'requesterId' - The ID of the requester that launched the instances on your behalf (for example, AWS Management Console or Auto Scaling).
-- * 'reservationId' - The ID of the reservation.
mkReservation ::
  -- | 'reservationId'
  Lude.Text ->
  -- | 'ownerId'
  Lude.Text ->
  Reservation
mkReservation pReservationId_ pOwnerId_ =
  Reservation'
    { groups = Lude.Nothing,
      instances = Lude.Nothing,
      requesterId = Lude.Nothing,
      reservationId = pReservationId_,
      ownerId = pOwnerId_
    }

-- | [EC2-Classic only] The security groups.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rGroups :: Lens.Lens' Reservation (Lude.Maybe [GroupIdentifier])
rGroups = Lens.lens (groups :: Reservation -> Lude.Maybe [GroupIdentifier]) (\s a -> s {groups = a} :: Reservation)
{-# DEPRECATED rGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | The instances.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rInstances :: Lens.Lens' Reservation (Lude.Maybe [Instance])
rInstances = Lens.lens (instances :: Reservation -> Lude.Maybe [Instance]) (\s a -> s {instances = a} :: Reservation)
{-# DEPRECATED rInstances "Use generic-lens or generic-optics with 'instances' instead." #-}

-- | The ID of the requester that launched the instances on your behalf (for example, AWS Management Console or Auto Scaling).
--
-- /Note:/ Consider using 'requesterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rRequesterId :: Lens.Lens' Reservation (Lude.Maybe Lude.Text)
rRequesterId = Lens.lens (requesterId :: Reservation -> Lude.Maybe Lude.Text) (\s a -> s {requesterId = a} :: Reservation)
{-# DEPRECATED rRequesterId "Use generic-lens or generic-optics with 'requesterId' instead." #-}

-- | The ID of the reservation.
--
-- /Note:/ Consider using 'reservationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rReservationId :: Lens.Lens' Reservation Lude.Text
rReservationId = Lens.lens (reservationId :: Reservation -> Lude.Text) (\s a -> s {reservationId = a} :: Reservation)
{-# DEPRECATED rReservationId "Use generic-lens or generic-optics with 'reservationId' instead." #-}

-- | The ID of the AWS account that owns the reservation.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rOwnerId :: Lens.Lens' Reservation Lude.Text
rOwnerId = Lens.lens (ownerId :: Reservation -> Lude.Text) (\s a -> s {ownerId = a} :: Reservation)
{-# DEPRECATED rOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

instance Lude.FromXML Reservation where
  parseXML x =
    Reservation'
      Lude.<$> ( x Lude..@? "groupSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> ( x Lude..@? "instancesSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "requesterId")
      Lude.<*> (x Lude..@ "reservationId")
      Lude.<*> (x Lude..@ "ownerId")
