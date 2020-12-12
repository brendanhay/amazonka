{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ReservationPlan
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ReservationPlan
  ( ReservationPlan (..),

    -- * Smart constructor
    mkReservationPlan,

    -- * Lenses
    rpStatus,
    rpExpiresAt,
    rpPurchasedAt,
    rpCommitment,
    rpReservedSlots,
    rpRenewalType,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.Commitment
import Network.AWS.MediaConvert.Types.RenewalType
import Network.AWS.MediaConvert.Types.ReservationPlanStatus
import qualified Network.AWS.Prelude as Lude

-- | Details about the pricing plan for your reserved queue. Required for reserved queues and not applicable to on-demand queues.
--
-- /See:/ 'mkReservationPlan' smart constructor.
data ReservationPlan = ReservationPlan'
  { status ::
      Lude.Maybe ReservationPlanStatus,
    expiresAt :: Lude.Maybe Lude.Timestamp,
    purchasedAt :: Lude.Maybe Lude.Timestamp,
    commitment :: Lude.Maybe Commitment,
    reservedSlots :: Lude.Maybe Lude.Int,
    renewalType :: Lude.Maybe RenewalType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReservationPlan' with the minimum fields required to make a request.
--
-- * 'commitment' - The length of the term of your reserved queue pricing plan commitment.
-- * 'expiresAt' - The timestamp in epoch seconds for when the current pricing plan term for this reserved queue expires.
-- * 'purchasedAt' - The timestamp in epoch seconds for when you set up the current pricing plan for this reserved queue.
-- * 'renewalType' - Specifies whether the term of your reserved queue pricing plan is automatically extended (AUTO_RENEW) or expires (EXPIRE) at the end of the term.
-- * 'reservedSlots' - Specifies the number of reserved transcode slots (RTS) for this queue. The number of RTS determines how many jobs the queue can process in parallel; each RTS can process one job at a time. When you increase this number, you extend your existing commitment with a new 12-month commitment for a larger number of RTS. The new commitment begins when you purchase the additional capacity. You can't decrease the number of RTS in your reserved queue.
-- * 'status' - Specifies whether the pricing plan for your reserved queue is ACTIVE or EXPIRED.
mkReservationPlan ::
  ReservationPlan
mkReservationPlan =
  ReservationPlan'
    { status = Lude.Nothing,
      expiresAt = Lude.Nothing,
      purchasedAt = Lude.Nothing,
      commitment = Lude.Nothing,
      reservedSlots = Lude.Nothing,
      renewalType = Lude.Nothing
    }

-- | Specifies whether the pricing plan for your reserved queue is ACTIVE or EXPIRED.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpStatus :: Lens.Lens' ReservationPlan (Lude.Maybe ReservationPlanStatus)
rpStatus = Lens.lens (status :: ReservationPlan -> Lude.Maybe ReservationPlanStatus) (\s a -> s {status = a} :: ReservationPlan)
{-# DEPRECATED rpStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The timestamp in epoch seconds for when the current pricing plan term for this reserved queue expires.
--
-- /Note:/ Consider using 'expiresAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpExpiresAt :: Lens.Lens' ReservationPlan (Lude.Maybe Lude.Timestamp)
rpExpiresAt = Lens.lens (expiresAt :: ReservationPlan -> Lude.Maybe Lude.Timestamp) (\s a -> s {expiresAt = a} :: ReservationPlan)
{-# DEPRECATED rpExpiresAt "Use generic-lens or generic-optics with 'expiresAt' instead." #-}

-- | The timestamp in epoch seconds for when you set up the current pricing plan for this reserved queue.
--
-- /Note:/ Consider using 'purchasedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpPurchasedAt :: Lens.Lens' ReservationPlan (Lude.Maybe Lude.Timestamp)
rpPurchasedAt = Lens.lens (purchasedAt :: ReservationPlan -> Lude.Maybe Lude.Timestamp) (\s a -> s {purchasedAt = a} :: ReservationPlan)
{-# DEPRECATED rpPurchasedAt "Use generic-lens or generic-optics with 'purchasedAt' instead." #-}

-- | The length of the term of your reserved queue pricing plan commitment.
--
-- /Note:/ Consider using 'commitment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpCommitment :: Lens.Lens' ReservationPlan (Lude.Maybe Commitment)
rpCommitment = Lens.lens (commitment :: ReservationPlan -> Lude.Maybe Commitment) (\s a -> s {commitment = a} :: ReservationPlan)
{-# DEPRECATED rpCommitment "Use generic-lens or generic-optics with 'commitment' instead." #-}

-- | Specifies the number of reserved transcode slots (RTS) for this queue. The number of RTS determines how many jobs the queue can process in parallel; each RTS can process one job at a time. When you increase this number, you extend your existing commitment with a new 12-month commitment for a larger number of RTS. The new commitment begins when you purchase the additional capacity. You can't decrease the number of RTS in your reserved queue.
--
-- /Note:/ Consider using 'reservedSlots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpReservedSlots :: Lens.Lens' ReservationPlan (Lude.Maybe Lude.Int)
rpReservedSlots = Lens.lens (reservedSlots :: ReservationPlan -> Lude.Maybe Lude.Int) (\s a -> s {reservedSlots = a} :: ReservationPlan)
{-# DEPRECATED rpReservedSlots "Use generic-lens or generic-optics with 'reservedSlots' instead." #-}

-- | Specifies whether the term of your reserved queue pricing plan is automatically extended (AUTO_RENEW) or expires (EXPIRE) at the end of the term.
--
-- /Note:/ Consider using 'renewalType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpRenewalType :: Lens.Lens' ReservationPlan (Lude.Maybe RenewalType)
rpRenewalType = Lens.lens (renewalType :: ReservationPlan -> Lude.Maybe RenewalType) (\s a -> s {renewalType = a} :: ReservationPlan)
{-# DEPRECATED rpRenewalType "Use generic-lens or generic-optics with 'renewalType' instead." #-}

instance Lude.FromJSON ReservationPlan where
  parseJSON =
    Lude.withObject
      "ReservationPlan"
      ( \x ->
          ReservationPlan'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "expiresAt")
            Lude.<*> (x Lude..:? "purchasedAt")
            Lude.<*> (x Lude..:? "commitment")
            Lude.<*> (x Lude..:? "reservedSlots")
            Lude.<*> (x Lude..:? "renewalType")
      )
