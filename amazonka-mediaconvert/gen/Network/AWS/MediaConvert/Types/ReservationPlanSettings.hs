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
-- Module      : Network.AWS.MediaConvert.Types.ReservationPlanSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ReservationPlanSettings where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.Commitment
import Network.AWS.MediaConvert.Types.RenewalType
import qualified Network.AWS.Prelude as Prelude

-- | Details about the pricing plan for your reserved queue. Required for
-- reserved queues and not applicable to on-demand queues.
--
-- /See:/ 'newReservationPlanSettings' smart constructor.
data ReservationPlanSettings = ReservationPlanSettings'
  { -- | The length of the term of your reserved queue pricing plan commitment.
    commitment :: Commitment,
    -- | Specifies the number of reserved transcode slots (RTS) for this queue.
    -- The number of RTS determines how many jobs the queue can process in
    -- parallel; each RTS can process one job at a time. You can\'t decrease
    -- the number of RTS in your reserved queue. You can increase the number of
    -- RTS by extending your existing commitment with a new 12-month commitment
    -- for the larger number. The new commitment begins when you purchase the
    -- additional capacity. You can\'t cancel your commitment or revert to your
    -- original commitment after you increase the capacity.
    reservedSlots :: Prelude.Int,
    -- | Specifies whether the term of your reserved queue pricing plan is
    -- automatically extended (AUTO_RENEW) or expires (EXPIRE) at the end of
    -- the term. When your term is auto renewed, you extend your commitment by
    -- 12 months from the auto renew date. You can cancel this commitment.
    renewalType :: RenewalType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ReservationPlanSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'commitment', 'reservationPlanSettings_commitment' - The length of the term of your reserved queue pricing plan commitment.
--
-- 'reservedSlots', 'reservationPlanSettings_reservedSlots' - Specifies the number of reserved transcode slots (RTS) for this queue.
-- The number of RTS determines how many jobs the queue can process in
-- parallel; each RTS can process one job at a time. You can\'t decrease
-- the number of RTS in your reserved queue. You can increase the number of
-- RTS by extending your existing commitment with a new 12-month commitment
-- for the larger number. The new commitment begins when you purchase the
-- additional capacity. You can\'t cancel your commitment or revert to your
-- original commitment after you increase the capacity.
--
-- 'renewalType', 'reservationPlanSettings_renewalType' - Specifies whether the term of your reserved queue pricing plan is
-- automatically extended (AUTO_RENEW) or expires (EXPIRE) at the end of
-- the term. When your term is auto renewed, you extend your commitment by
-- 12 months from the auto renew date. You can cancel this commitment.
newReservationPlanSettings ::
  -- | 'commitment'
  Commitment ->
  -- | 'reservedSlots'
  Prelude.Int ->
  -- | 'renewalType'
  RenewalType ->
  ReservationPlanSettings
newReservationPlanSettings
  pCommitment_
  pReservedSlots_
  pRenewalType_ =
    ReservationPlanSettings'
      { commitment = pCommitment_,
        reservedSlots = pReservedSlots_,
        renewalType = pRenewalType_
      }

-- | The length of the term of your reserved queue pricing plan commitment.
reservationPlanSettings_commitment :: Lens.Lens' ReservationPlanSettings Commitment
reservationPlanSettings_commitment = Lens.lens (\ReservationPlanSettings' {commitment} -> commitment) (\s@ReservationPlanSettings' {} a -> s {commitment = a} :: ReservationPlanSettings)

-- | Specifies the number of reserved transcode slots (RTS) for this queue.
-- The number of RTS determines how many jobs the queue can process in
-- parallel; each RTS can process one job at a time. You can\'t decrease
-- the number of RTS in your reserved queue. You can increase the number of
-- RTS by extending your existing commitment with a new 12-month commitment
-- for the larger number. The new commitment begins when you purchase the
-- additional capacity. You can\'t cancel your commitment or revert to your
-- original commitment after you increase the capacity.
reservationPlanSettings_reservedSlots :: Lens.Lens' ReservationPlanSettings Prelude.Int
reservationPlanSettings_reservedSlots = Lens.lens (\ReservationPlanSettings' {reservedSlots} -> reservedSlots) (\s@ReservationPlanSettings' {} a -> s {reservedSlots = a} :: ReservationPlanSettings)

-- | Specifies whether the term of your reserved queue pricing plan is
-- automatically extended (AUTO_RENEW) or expires (EXPIRE) at the end of
-- the term. When your term is auto renewed, you extend your commitment by
-- 12 months from the auto renew date. You can cancel this commitment.
reservationPlanSettings_renewalType :: Lens.Lens' ReservationPlanSettings RenewalType
reservationPlanSettings_renewalType = Lens.lens (\ReservationPlanSettings' {renewalType} -> renewalType) (\s@ReservationPlanSettings' {} a -> s {renewalType = a} :: ReservationPlanSettings)

instance Prelude.Hashable ReservationPlanSettings

instance Prelude.NFData ReservationPlanSettings

instance Prelude.ToJSON ReservationPlanSettings where
  toJSON ReservationPlanSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("commitment" Prelude..= commitment),
            Prelude.Just
              ("reservedSlots" Prelude..= reservedSlots),
            Prelude.Just ("renewalType" Prelude..= renewalType)
          ]
      )
