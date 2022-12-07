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
-- Module      : Amazonka.EC2.Types.Reservation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.Reservation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.GroupIdentifier
import Amazonka.EC2.Types.Instance
import qualified Amazonka.Prelude as Prelude

-- | Describes a launch request for one or more instances, and includes
-- owner, requester, and security group information that applies to all
-- instances in the launch request.
--
-- /See:/ 'newReservation' smart constructor.
data Reservation = Reservation'
  { -- | The instances.
    instances :: Prelude.Maybe [Instance],
    -- | [EC2-Classic only] The security groups.
    groups :: Prelude.Maybe [GroupIdentifier],
    -- | The ID of the requester that launched the instances on your behalf (for
    -- example, Amazon Web Services Management Console or Auto Scaling).
    requesterId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the reservation.
    reservationId :: Prelude.Text,
    -- | The ID of the Amazon Web Services account that owns the reservation.
    ownerId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Reservation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instances', 'reservation_instances' - The instances.
--
-- 'groups', 'reservation_groups' - [EC2-Classic only] The security groups.
--
-- 'requesterId', 'reservation_requesterId' - The ID of the requester that launched the instances on your behalf (for
-- example, Amazon Web Services Management Console or Auto Scaling).
--
-- 'reservationId', 'reservation_reservationId' - The ID of the reservation.
--
-- 'ownerId', 'reservation_ownerId' - The ID of the Amazon Web Services account that owns the reservation.
newReservation ::
  -- | 'reservationId'
  Prelude.Text ->
  -- | 'ownerId'
  Prelude.Text ->
  Reservation
newReservation pReservationId_ pOwnerId_ =
  Reservation'
    { instances = Prelude.Nothing,
      groups = Prelude.Nothing,
      requesterId = Prelude.Nothing,
      reservationId = pReservationId_,
      ownerId = pOwnerId_
    }

-- | The instances.
reservation_instances :: Lens.Lens' Reservation (Prelude.Maybe [Instance])
reservation_instances = Lens.lens (\Reservation' {instances} -> instances) (\s@Reservation' {} a -> s {instances = a} :: Reservation) Prelude.. Lens.mapping Lens.coerced

-- | [EC2-Classic only] The security groups.
reservation_groups :: Lens.Lens' Reservation (Prelude.Maybe [GroupIdentifier])
reservation_groups = Lens.lens (\Reservation' {groups} -> groups) (\s@Reservation' {} a -> s {groups = a} :: Reservation) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the requester that launched the instances on your behalf (for
-- example, Amazon Web Services Management Console or Auto Scaling).
reservation_requesterId :: Lens.Lens' Reservation (Prelude.Maybe Prelude.Text)
reservation_requesterId = Lens.lens (\Reservation' {requesterId} -> requesterId) (\s@Reservation' {} a -> s {requesterId = a} :: Reservation)

-- | The ID of the reservation.
reservation_reservationId :: Lens.Lens' Reservation Prelude.Text
reservation_reservationId = Lens.lens (\Reservation' {reservationId} -> reservationId) (\s@Reservation' {} a -> s {reservationId = a} :: Reservation)

-- | The ID of the Amazon Web Services account that owns the reservation.
reservation_ownerId :: Lens.Lens' Reservation Prelude.Text
reservation_ownerId = Lens.lens (\Reservation' {ownerId} -> ownerId) (\s@Reservation' {} a -> s {ownerId = a} :: Reservation)

instance Data.FromXML Reservation where
  parseXML x =
    Reservation'
      Prelude.<$> ( x Data..@? "instancesSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x Data..@? "groupSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "requesterId")
      Prelude.<*> (x Data..@ "reservationId")
      Prelude.<*> (x Data..@ "ownerId")

instance Prelude.Hashable Reservation where
  hashWithSalt _salt Reservation' {..} =
    _salt `Prelude.hashWithSalt` instances
      `Prelude.hashWithSalt` groups
      `Prelude.hashWithSalt` requesterId
      `Prelude.hashWithSalt` reservationId
      `Prelude.hashWithSalt` ownerId

instance Prelude.NFData Reservation where
  rnf Reservation' {..} =
    Prelude.rnf instances
      `Prelude.seq` Prelude.rnf groups
      `Prelude.seq` Prelude.rnf requesterId
      `Prelude.seq` Prelude.rnf reservationId
      `Prelude.seq` Prelude.rnf ownerId
