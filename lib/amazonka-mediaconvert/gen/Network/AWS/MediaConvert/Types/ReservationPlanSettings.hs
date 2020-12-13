{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ReservationPlanSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.ReservationPlanSettings
  ( ReservationPlanSettings (..),

    -- * Smart constructor
    mkReservationPlanSettings,

    -- * Lenses
    rpsCommitment,
    rpsReservedSlots,
    rpsRenewalType,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types.Commitment
import Network.AWS.MediaConvert.Types.RenewalType
import qualified Network.AWS.Prelude as Lude

-- | Details about the pricing plan for your reserved queue. Required for reserved queues and not applicable to on-demand queues.
--
-- /See:/ 'mkReservationPlanSettings' smart constructor.
data ReservationPlanSettings = ReservationPlanSettings'
  { -- | The length of the term of your reserved queue pricing plan commitment.
    commitment :: Commitment,
    -- | Specifies the number of reserved transcode slots (RTS) for this queue. The number of RTS determines how many jobs the queue can process in parallel; each RTS can process one job at a time. You can't decrease the number of RTS in your reserved queue. You can increase the number of RTS by extending your existing commitment with a new 12-month commitment for the larger number. The new commitment begins when you purchase the additional capacity. You can't cancel your commitment or revert to your original commitment after you increase the capacity.
    reservedSlots :: Lude.Int,
    -- | Specifies whether the term of your reserved queue pricing plan is automatically extended (AUTO_RENEW) or expires (EXPIRE) at the end of the term. When your term is auto renewed, you extend your commitment by 12 months from the auto renew date. You can cancel this commitment.
    renewalType :: RenewalType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReservationPlanSettings' with the minimum fields required to make a request.
--
-- * 'commitment' - The length of the term of your reserved queue pricing plan commitment.
-- * 'reservedSlots' - Specifies the number of reserved transcode slots (RTS) for this queue. The number of RTS determines how many jobs the queue can process in parallel; each RTS can process one job at a time. You can't decrease the number of RTS in your reserved queue. You can increase the number of RTS by extending your existing commitment with a new 12-month commitment for the larger number. The new commitment begins when you purchase the additional capacity. You can't cancel your commitment or revert to your original commitment after you increase the capacity.
-- * 'renewalType' - Specifies whether the term of your reserved queue pricing plan is automatically extended (AUTO_RENEW) or expires (EXPIRE) at the end of the term. When your term is auto renewed, you extend your commitment by 12 months from the auto renew date. You can cancel this commitment.
mkReservationPlanSettings ::
  -- | 'commitment'
  Commitment ->
  -- | 'reservedSlots'
  Lude.Int ->
  -- | 'renewalType'
  RenewalType ->
  ReservationPlanSettings
mkReservationPlanSettings
  pCommitment_
  pReservedSlots_
  pRenewalType_ =
    ReservationPlanSettings'
      { commitment = pCommitment_,
        reservedSlots = pReservedSlots_,
        renewalType = pRenewalType_
      }

-- | The length of the term of your reserved queue pricing plan commitment.
--
-- /Note:/ Consider using 'commitment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpsCommitment :: Lens.Lens' ReservationPlanSettings Commitment
rpsCommitment = Lens.lens (commitment :: ReservationPlanSettings -> Commitment) (\s a -> s {commitment = a} :: ReservationPlanSettings)
{-# DEPRECATED rpsCommitment "Use generic-lens or generic-optics with 'commitment' instead." #-}

-- | Specifies the number of reserved transcode slots (RTS) for this queue. The number of RTS determines how many jobs the queue can process in parallel; each RTS can process one job at a time. You can't decrease the number of RTS in your reserved queue. You can increase the number of RTS by extending your existing commitment with a new 12-month commitment for the larger number. The new commitment begins when you purchase the additional capacity. You can't cancel your commitment or revert to your original commitment after you increase the capacity.
--
-- /Note:/ Consider using 'reservedSlots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpsReservedSlots :: Lens.Lens' ReservationPlanSettings Lude.Int
rpsReservedSlots = Lens.lens (reservedSlots :: ReservationPlanSettings -> Lude.Int) (\s a -> s {reservedSlots = a} :: ReservationPlanSettings)
{-# DEPRECATED rpsReservedSlots "Use generic-lens or generic-optics with 'reservedSlots' instead." #-}

-- | Specifies whether the term of your reserved queue pricing plan is automatically extended (AUTO_RENEW) or expires (EXPIRE) at the end of the term. When your term is auto renewed, you extend your commitment by 12 months from the auto renew date. You can cancel this commitment.
--
-- /Note:/ Consider using 'renewalType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpsRenewalType :: Lens.Lens' ReservationPlanSettings RenewalType
rpsRenewalType = Lens.lens (renewalType :: ReservationPlanSettings -> RenewalType) (\s a -> s {renewalType = a} :: ReservationPlanSettings)
{-# DEPRECATED rpsRenewalType "Use generic-lens or generic-optics with 'renewalType' instead." #-}

instance Lude.ToJSON ReservationPlanSettings where
  toJSON ReservationPlanSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("commitment" Lude..= commitment),
            Lude.Just ("reservedSlots" Lude..= reservedSlots),
            Lude.Just ("renewalType" Lude..= renewalType)
          ]
      )
