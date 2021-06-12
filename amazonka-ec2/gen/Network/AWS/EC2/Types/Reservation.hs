{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Reservation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Reservation where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.GroupIdentifier
import Network.AWS.EC2.Types.Instance
import qualified Network.AWS.Lens as Lens

-- | Describes a launch request for one or more instances, and includes
-- owner, requester, and security group information that applies to all
-- instances in the launch request.
--
-- /See:/ 'newReservation' smart constructor.
data Reservation = Reservation'
  { -- | [EC2-Classic only] The security groups.
    groups :: Core.Maybe [GroupIdentifier],
    -- | The ID of the requester that launched the instances on your behalf (for
    -- example, AWS Management Console or Auto Scaling).
    requesterId :: Core.Maybe Core.Text,
    -- | The instances.
    instances :: Core.Maybe [Instance],
    -- | The ID of the reservation.
    reservationId :: Core.Text,
    -- | The ID of the AWS account that owns the reservation.
    ownerId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Reservation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groups', 'reservation_groups' - [EC2-Classic only] The security groups.
--
-- 'requesterId', 'reservation_requesterId' - The ID of the requester that launched the instances on your behalf (for
-- example, AWS Management Console or Auto Scaling).
--
-- 'instances', 'reservation_instances' - The instances.
--
-- 'reservationId', 'reservation_reservationId' - The ID of the reservation.
--
-- 'ownerId', 'reservation_ownerId' - The ID of the AWS account that owns the reservation.
newReservation ::
  -- | 'reservationId'
  Core.Text ->
  -- | 'ownerId'
  Core.Text ->
  Reservation
newReservation pReservationId_ pOwnerId_ =
  Reservation'
    { groups = Core.Nothing,
      requesterId = Core.Nothing,
      instances = Core.Nothing,
      reservationId = pReservationId_,
      ownerId = pOwnerId_
    }

-- | [EC2-Classic only] The security groups.
reservation_groups :: Lens.Lens' Reservation (Core.Maybe [GroupIdentifier])
reservation_groups = Lens.lens (\Reservation' {groups} -> groups) (\s@Reservation' {} a -> s {groups = a} :: Reservation) Core.. Lens.mapping Lens._Coerce

-- | The ID of the requester that launched the instances on your behalf (for
-- example, AWS Management Console or Auto Scaling).
reservation_requesterId :: Lens.Lens' Reservation (Core.Maybe Core.Text)
reservation_requesterId = Lens.lens (\Reservation' {requesterId} -> requesterId) (\s@Reservation' {} a -> s {requesterId = a} :: Reservation)

-- | The instances.
reservation_instances :: Lens.Lens' Reservation (Core.Maybe [Instance])
reservation_instances = Lens.lens (\Reservation' {instances} -> instances) (\s@Reservation' {} a -> s {instances = a} :: Reservation) Core.. Lens.mapping Lens._Coerce

-- | The ID of the reservation.
reservation_reservationId :: Lens.Lens' Reservation Core.Text
reservation_reservationId = Lens.lens (\Reservation' {reservationId} -> reservationId) (\s@Reservation' {} a -> s {reservationId = a} :: Reservation)

-- | The ID of the AWS account that owns the reservation.
reservation_ownerId :: Lens.Lens' Reservation Core.Text
reservation_ownerId = Lens.lens (\Reservation' {ownerId} -> ownerId) (\s@Reservation' {} a -> s {ownerId = a} :: Reservation)

instance Core.FromXML Reservation where
  parseXML x =
    Reservation'
      Core.<$> ( x Core..@? "groupSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "requesterId")
      Core.<*> ( x Core..@? "instancesSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@ "reservationId")
      Core.<*> (x Core..@ "ownerId")

instance Core.Hashable Reservation

instance Core.NFData Reservation
