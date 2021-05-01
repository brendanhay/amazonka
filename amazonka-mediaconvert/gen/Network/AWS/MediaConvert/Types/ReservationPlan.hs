{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MediaConvert.Types.ReservationPlan
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ReservationPlan where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.Commitment
import Network.AWS.MediaConvert.Types.RenewalType
import Network.AWS.MediaConvert.Types.ReservationPlanStatus
import qualified Network.AWS.Prelude as Prelude

-- | Details about the pricing plan for your reserved queue. Required for
-- reserved queues and not applicable to on-demand queues.
--
-- /See:/ 'newReservationPlan' smart constructor.
data ReservationPlan = ReservationPlan'
  { -- | Specifies whether the pricing plan for your reserved queue is ACTIVE or
    -- EXPIRED.
    status :: Prelude.Maybe ReservationPlanStatus,
    -- | Specifies the number of reserved transcode slots (RTS) for this queue.
    -- The number of RTS determines how many jobs the queue can process in
    -- parallel; each RTS can process one job at a time. When you increase this
    -- number, you extend your existing commitment with a new 12-month
    -- commitment for a larger number of RTS. The new commitment begins when
    -- you purchase the additional capacity. You can\'t decrease the number of
    -- RTS in your reserved queue.
    reservedSlots :: Prelude.Maybe Prelude.Int,
    -- | The timestamp in epoch seconds for when the current pricing plan term
    -- for this reserved queue expires.
    expiresAt :: Prelude.Maybe Prelude.POSIX,
    -- | The timestamp in epoch seconds for when you set up the current pricing
    -- plan for this reserved queue.
    purchasedAt :: Prelude.Maybe Prelude.POSIX,
    -- | Specifies whether the term of your reserved queue pricing plan is
    -- automatically extended (AUTO_RENEW) or expires (EXPIRE) at the end of
    -- the term.
    renewalType :: Prelude.Maybe RenewalType,
    -- | The length of the term of your reserved queue pricing plan commitment.
    commitment :: Prelude.Maybe Commitment
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ReservationPlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'reservationPlan_status' - Specifies whether the pricing plan for your reserved queue is ACTIVE or
-- EXPIRED.
--
-- 'reservedSlots', 'reservationPlan_reservedSlots' - Specifies the number of reserved transcode slots (RTS) for this queue.
-- The number of RTS determines how many jobs the queue can process in
-- parallel; each RTS can process one job at a time. When you increase this
-- number, you extend your existing commitment with a new 12-month
-- commitment for a larger number of RTS. The new commitment begins when
-- you purchase the additional capacity. You can\'t decrease the number of
-- RTS in your reserved queue.
--
-- 'expiresAt', 'reservationPlan_expiresAt' - The timestamp in epoch seconds for when the current pricing plan term
-- for this reserved queue expires.
--
-- 'purchasedAt', 'reservationPlan_purchasedAt' - The timestamp in epoch seconds for when you set up the current pricing
-- plan for this reserved queue.
--
-- 'renewalType', 'reservationPlan_renewalType' - Specifies whether the term of your reserved queue pricing plan is
-- automatically extended (AUTO_RENEW) or expires (EXPIRE) at the end of
-- the term.
--
-- 'commitment', 'reservationPlan_commitment' - The length of the term of your reserved queue pricing plan commitment.
newReservationPlan ::
  ReservationPlan
newReservationPlan =
  ReservationPlan'
    { status = Prelude.Nothing,
      reservedSlots = Prelude.Nothing,
      expiresAt = Prelude.Nothing,
      purchasedAt = Prelude.Nothing,
      renewalType = Prelude.Nothing,
      commitment = Prelude.Nothing
    }

-- | Specifies whether the pricing plan for your reserved queue is ACTIVE or
-- EXPIRED.
reservationPlan_status :: Lens.Lens' ReservationPlan (Prelude.Maybe ReservationPlanStatus)
reservationPlan_status = Lens.lens (\ReservationPlan' {status} -> status) (\s@ReservationPlan' {} a -> s {status = a} :: ReservationPlan)

-- | Specifies the number of reserved transcode slots (RTS) for this queue.
-- The number of RTS determines how many jobs the queue can process in
-- parallel; each RTS can process one job at a time. When you increase this
-- number, you extend your existing commitment with a new 12-month
-- commitment for a larger number of RTS. The new commitment begins when
-- you purchase the additional capacity. You can\'t decrease the number of
-- RTS in your reserved queue.
reservationPlan_reservedSlots :: Lens.Lens' ReservationPlan (Prelude.Maybe Prelude.Int)
reservationPlan_reservedSlots = Lens.lens (\ReservationPlan' {reservedSlots} -> reservedSlots) (\s@ReservationPlan' {} a -> s {reservedSlots = a} :: ReservationPlan)

-- | The timestamp in epoch seconds for when the current pricing plan term
-- for this reserved queue expires.
reservationPlan_expiresAt :: Lens.Lens' ReservationPlan (Prelude.Maybe Prelude.UTCTime)
reservationPlan_expiresAt = Lens.lens (\ReservationPlan' {expiresAt} -> expiresAt) (\s@ReservationPlan' {} a -> s {expiresAt = a} :: ReservationPlan) Prelude.. Lens.mapping Prelude._Time

-- | The timestamp in epoch seconds for when you set up the current pricing
-- plan for this reserved queue.
reservationPlan_purchasedAt :: Lens.Lens' ReservationPlan (Prelude.Maybe Prelude.UTCTime)
reservationPlan_purchasedAt = Lens.lens (\ReservationPlan' {purchasedAt} -> purchasedAt) (\s@ReservationPlan' {} a -> s {purchasedAt = a} :: ReservationPlan) Prelude.. Lens.mapping Prelude._Time

-- | Specifies whether the term of your reserved queue pricing plan is
-- automatically extended (AUTO_RENEW) or expires (EXPIRE) at the end of
-- the term.
reservationPlan_renewalType :: Lens.Lens' ReservationPlan (Prelude.Maybe RenewalType)
reservationPlan_renewalType = Lens.lens (\ReservationPlan' {renewalType} -> renewalType) (\s@ReservationPlan' {} a -> s {renewalType = a} :: ReservationPlan)

-- | The length of the term of your reserved queue pricing plan commitment.
reservationPlan_commitment :: Lens.Lens' ReservationPlan (Prelude.Maybe Commitment)
reservationPlan_commitment = Lens.lens (\ReservationPlan' {commitment} -> commitment) (\s@ReservationPlan' {} a -> s {commitment = a} :: ReservationPlan)

instance Prelude.FromJSON ReservationPlan where
  parseJSON =
    Prelude.withObject
      "ReservationPlan"
      ( \x ->
          ReservationPlan'
            Prelude.<$> (x Prelude..:? "status")
            Prelude.<*> (x Prelude..:? "reservedSlots")
            Prelude.<*> (x Prelude..:? "expiresAt")
            Prelude.<*> (x Prelude..:? "purchasedAt")
            Prelude.<*> (x Prelude..:? "renewalType")
            Prelude.<*> (x Prelude..:? "commitment")
      )

instance Prelude.Hashable ReservationPlan

instance Prelude.NFData ReservationPlan
