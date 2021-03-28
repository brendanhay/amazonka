{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.ReservationPlanSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.ReservationPlanSettings
  ( ReservationPlanSettings (..)
  -- * Smart constructor
  , mkReservationPlanSettings
  -- * Lenses
  , rpsCommitment
  , rpsReservedSlots
  , rpsRenewalType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.Commitment as Types
import qualified Network.AWS.MediaConvert.Types.RenewalType as Types
import qualified Network.AWS.Prelude as Core

-- | Details about the pricing plan for your reserved queue. Required for reserved queues and not applicable to on-demand queues.
--
-- /See:/ 'mkReservationPlanSettings' smart constructor.
data ReservationPlanSettings = ReservationPlanSettings'
  { commitment :: Types.Commitment
    -- ^ The length of the term of your reserved queue pricing plan commitment.
  , reservedSlots :: Core.Int
    -- ^ Specifies the number of reserved transcode slots (RTS) for this queue. The number of RTS determines how many jobs the queue can process in parallel; each RTS can process one job at a time. You can't decrease the number of RTS in your reserved queue. You can increase the number of RTS by extending your existing commitment with a new 12-month commitment for the larger number. The new commitment begins when you purchase the additional capacity. You can't cancel your commitment or revert to your original commitment after you increase the capacity.
  , renewalType :: Types.RenewalType
    -- ^ Specifies whether the term of your reserved queue pricing plan is automatically extended (AUTO_RENEW) or expires (EXPIRE) at the end of the term. When your term is auto renewed, you extend your commitment by 12 months from the auto renew date. You can cancel this commitment.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReservationPlanSettings' value with any optional fields omitted.
mkReservationPlanSettings
    :: Types.Commitment -- ^ 'commitment'
    -> Core.Int -- ^ 'reservedSlots'
    -> Types.RenewalType -- ^ 'renewalType'
    -> ReservationPlanSettings
mkReservationPlanSettings commitment reservedSlots renewalType
  = ReservationPlanSettings'{commitment, reservedSlots, renewalType}

-- | The length of the term of your reserved queue pricing plan commitment.
--
-- /Note:/ Consider using 'commitment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpsCommitment :: Lens.Lens' ReservationPlanSettings Types.Commitment
rpsCommitment = Lens.field @"commitment"
{-# INLINEABLE rpsCommitment #-}
{-# DEPRECATED commitment "Use generic-lens or generic-optics with 'commitment' instead"  #-}

-- | Specifies the number of reserved transcode slots (RTS) for this queue. The number of RTS determines how many jobs the queue can process in parallel; each RTS can process one job at a time. You can't decrease the number of RTS in your reserved queue. You can increase the number of RTS by extending your existing commitment with a new 12-month commitment for the larger number. The new commitment begins when you purchase the additional capacity. You can't cancel your commitment or revert to your original commitment after you increase the capacity.
--
-- /Note:/ Consider using 'reservedSlots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpsReservedSlots :: Lens.Lens' ReservationPlanSettings Core.Int
rpsReservedSlots = Lens.field @"reservedSlots"
{-# INLINEABLE rpsReservedSlots #-}
{-# DEPRECATED reservedSlots "Use generic-lens or generic-optics with 'reservedSlots' instead"  #-}

-- | Specifies whether the term of your reserved queue pricing plan is automatically extended (AUTO_RENEW) or expires (EXPIRE) at the end of the term. When your term is auto renewed, you extend your commitment by 12 months from the auto renew date. You can cancel this commitment.
--
-- /Note:/ Consider using 'renewalType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpsRenewalType :: Lens.Lens' ReservationPlanSettings Types.RenewalType
rpsRenewalType = Lens.field @"renewalType"
{-# INLINEABLE rpsRenewalType #-}
{-# DEPRECATED renewalType "Use generic-lens or generic-optics with 'renewalType' instead"  #-}

instance Core.FromJSON ReservationPlanSettings where
        toJSON ReservationPlanSettings{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("commitment" Core..= commitment),
                  Core.Just ("reservedSlots" Core..= reservedSlots),
                  Core.Just ("renewalType" Core..= renewalType)])
