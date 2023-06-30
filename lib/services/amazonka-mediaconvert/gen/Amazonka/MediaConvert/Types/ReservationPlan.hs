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
-- Module      : Amazonka.MediaConvert.Types.ReservationPlan
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.ReservationPlan where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaConvert.Types.Commitment
import Amazonka.MediaConvert.Types.RenewalType
import Amazonka.MediaConvert.Types.ReservationPlanStatus
import qualified Amazonka.Prelude as Prelude

-- | Details about the pricing plan for your reserved queue. Required for
-- reserved queues and not applicable to on-demand queues.
--
-- /See:/ 'newReservationPlan' smart constructor.
data ReservationPlan = ReservationPlan'
  { -- | The length of the term of your reserved queue pricing plan commitment.
    commitment :: Prelude.Maybe Commitment,
    -- | The timestamp in epoch seconds for when the current pricing plan term
    -- for this reserved queue expires.
    expiresAt :: Prelude.Maybe Data.POSIX,
    -- | The timestamp in epoch seconds for when you set up the current pricing
    -- plan for this reserved queue.
    purchasedAt :: Prelude.Maybe Data.POSIX,
    -- | Specifies whether the term of your reserved queue pricing plan is
    -- automatically extended (AUTO_RENEW) or expires (EXPIRE) at the end of
    -- the term.
    renewalType :: Prelude.Maybe RenewalType,
    -- | Specifies the number of reserved transcode slots (RTS) for this queue.
    -- The number of RTS determines how many jobs the queue can process in
    -- parallel; each RTS can process one job at a time. When you increase this
    -- number, you extend your existing commitment with a new 12-month
    -- commitment for a larger number of RTS. The new commitment begins when
    -- you purchase the additional capacity. You can\'t decrease the number of
    -- RTS in your reserved queue.
    reservedSlots :: Prelude.Maybe Prelude.Int,
    -- | Specifies whether the pricing plan for your reserved queue is ACTIVE or
    -- EXPIRED.
    status :: Prelude.Maybe ReservationPlanStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReservationPlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'commitment', 'reservationPlan_commitment' - The length of the term of your reserved queue pricing plan commitment.
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
-- 'reservedSlots', 'reservationPlan_reservedSlots' - Specifies the number of reserved transcode slots (RTS) for this queue.
-- The number of RTS determines how many jobs the queue can process in
-- parallel; each RTS can process one job at a time. When you increase this
-- number, you extend your existing commitment with a new 12-month
-- commitment for a larger number of RTS. The new commitment begins when
-- you purchase the additional capacity. You can\'t decrease the number of
-- RTS in your reserved queue.
--
-- 'status', 'reservationPlan_status' - Specifies whether the pricing plan for your reserved queue is ACTIVE or
-- EXPIRED.
newReservationPlan ::
  ReservationPlan
newReservationPlan =
  ReservationPlan'
    { commitment = Prelude.Nothing,
      expiresAt = Prelude.Nothing,
      purchasedAt = Prelude.Nothing,
      renewalType = Prelude.Nothing,
      reservedSlots = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The length of the term of your reserved queue pricing plan commitment.
reservationPlan_commitment :: Lens.Lens' ReservationPlan (Prelude.Maybe Commitment)
reservationPlan_commitment = Lens.lens (\ReservationPlan' {commitment} -> commitment) (\s@ReservationPlan' {} a -> s {commitment = a} :: ReservationPlan)

-- | The timestamp in epoch seconds for when the current pricing plan term
-- for this reserved queue expires.
reservationPlan_expiresAt :: Lens.Lens' ReservationPlan (Prelude.Maybe Prelude.UTCTime)
reservationPlan_expiresAt = Lens.lens (\ReservationPlan' {expiresAt} -> expiresAt) (\s@ReservationPlan' {} a -> s {expiresAt = a} :: ReservationPlan) Prelude.. Lens.mapping Data._Time

-- | The timestamp in epoch seconds for when you set up the current pricing
-- plan for this reserved queue.
reservationPlan_purchasedAt :: Lens.Lens' ReservationPlan (Prelude.Maybe Prelude.UTCTime)
reservationPlan_purchasedAt = Lens.lens (\ReservationPlan' {purchasedAt} -> purchasedAt) (\s@ReservationPlan' {} a -> s {purchasedAt = a} :: ReservationPlan) Prelude.. Lens.mapping Data._Time

-- | Specifies whether the term of your reserved queue pricing plan is
-- automatically extended (AUTO_RENEW) or expires (EXPIRE) at the end of
-- the term.
reservationPlan_renewalType :: Lens.Lens' ReservationPlan (Prelude.Maybe RenewalType)
reservationPlan_renewalType = Lens.lens (\ReservationPlan' {renewalType} -> renewalType) (\s@ReservationPlan' {} a -> s {renewalType = a} :: ReservationPlan)

-- | Specifies the number of reserved transcode slots (RTS) for this queue.
-- The number of RTS determines how many jobs the queue can process in
-- parallel; each RTS can process one job at a time. When you increase this
-- number, you extend your existing commitment with a new 12-month
-- commitment for a larger number of RTS. The new commitment begins when
-- you purchase the additional capacity. You can\'t decrease the number of
-- RTS in your reserved queue.
reservationPlan_reservedSlots :: Lens.Lens' ReservationPlan (Prelude.Maybe Prelude.Int)
reservationPlan_reservedSlots = Lens.lens (\ReservationPlan' {reservedSlots} -> reservedSlots) (\s@ReservationPlan' {} a -> s {reservedSlots = a} :: ReservationPlan)

-- | Specifies whether the pricing plan for your reserved queue is ACTIVE or
-- EXPIRED.
reservationPlan_status :: Lens.Lens' ReservationPlan (Prelude.Maybe ReservationPlanStatus)
reservationPlan_status = Lens.lens (\ReservationPlan' {status} -> status) (\s@ReservationPlan' {} a -> s {status = a} :: ReservationPlan)

instance Data.FromJSON ReservationPlan where
  parseJSON =
    Data.withObject
      "ReservationPlan"
      ( \x ->
          ReservationPlan'
            Prelude.<$> (x Data..:? "commitment")
            Prelude.<*> (x Data..:? "expiresAt")
            Prelude.<*> (x Data..:? "purchasedAt")
            Prelude.<*> (x Data..:? "renewalType")
            Prelude.<*> (x Data..:? "reservedSlots")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable ReservationPlan where
  hashWithSalt _salt ReservationPlan' {..} =
    _salt
      `Prelude.hashWithSalt` commitment
      `Prelude.hashWithSalt` expiresAt
      `Prelude.hashWithSalt` purchasedAt
      `Prelude.hashWithSalt` renewalType
      `Prelude.hashWithSalt` reservedSlots
      `Prelude.hashWithSalt` status

instance Prelude.NFData ReservationPlan where
  rnf ReservationPlan' {..} =
    Prelude.rnf commitment
      `Prelude.seq` Prelude.rnf expiresAt
      `Prelude.seq` Prelude.rnf purchasedAt
      `Prelude.seq` Prelude.rnf renewalType
      `Prelude.seq` Prelude.rnf reservedSlots
      `Prelude.seq` Prelude.rnf status
