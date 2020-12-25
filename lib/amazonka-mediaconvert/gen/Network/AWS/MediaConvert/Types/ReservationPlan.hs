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
    rpCommitment,
    rpExpiresAt,
    rpPurchasedAt,
    rpRenewalType,
    rpReservedSlots,
    rpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.Commitment as Types
import qualified Network.AWS.MediaConvert.Types.RenewalType as Types
import qualified Network.AWS.MediaConvert.Types.ReservationPlanStatus as Types
import qualified Network.AWS.Prelude as Core

-- | Details about the pricing plan for your reserved queue. Required for reserved queues and not applicable to on-demand queues.
--
-- /See:/ 'mkReservationPlan' smart constructor.
data ReservationPlan = ReservationPlan'
  { -- | The length of the term of your reserved queue pricing plan commitment.
    commitment :: Core.Maybe Types.Commitment,
    -- | The timestamp in epoch seconds for when the current pricing plan term for this reserved queue expires.
    expiresAt :: Core.Maybe Core.NominalDiffTime,
    -- | The timestamp in epoch seconds for when you set up the current pricing plan for this reserved queue.
    purchasedAt :: Core.Maybe Core.NominalDiffTime,
    -- | Specifies whether the term of your reserved queue pricing plan is automatically extended (AUTO_RENEW) or expires (EXPIRE) at the end of the term.
    renewalType :: Core.Maybe Types.RenewalType,
    -- | Specifies the number of reserved transcode slots (RTS) for this queue. The number of RTS determines how many jobs the queue can process in parallel; each RTS can process one job at a time. When you increase this number, you extend your existing commitment with a new 12-month commitment for a larger number of RTS. The new commitment begins when you purchase the additional capacity. You can't decrease the number of RTS in your reserved queue.
    reservedSlots :: Core.Maybe Core.Int,
    -- | Specifies whether the pricing plan for your reserved queue is ACTIVE or EXPIRED.
    status :: Core.Maybe Types.ReservationPlanStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ReservationPlan' value with any optional fields omitted.
mkReservationPlan ::
  ReservationPlan
mkReservationPlan =
  ReservationPlan'
    { commitment = Core.Nothing,
      expiresAt = Core.Nothing,
      purchasedAt = Core.Nothing,
      renewalType = Core.Nothing,
      reservedSlots = Core.Nothing,
      status = Core.Nothing
    }

-- | The length of the term of your reserved queue pricing plan commitment.
--
-- /Note:/ Consider using 'commitment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpCommitment :: Lens.Lens' ReservationPlan (Core.Maybe Types.Commitment)
rpCommitment = Lens.field @"commitment"
{-# DEPRECATED rpCommitment "Use generic-lens or generic-optics with 'commitment' instead." #-}

-- | The timestamp in epoch seconds for when the current pricing plan term for this reserved queue expires.
--
-- /Note:/ Consider using 'expiresAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpExpiresAt :: Lens.Lens' ReservationPlan (Core.Maybe Core.NominalDiffTime)
rpExpiresAt = Lens.field @"expiresAt"
{-# DEPRECATED rpExpiresAt "Use generic-lens or generic-optics with 'expiresAt' instead." #-}

-- | The timestamp in epoch seconds for when you set up the current pricing plan for this reserved queue.
--
-- /Note:/ Consider using 'purchasedAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpPurchasedAt :: Lens.Lens' ReservationPlan (Core.Maybe Core.NominalDiffTime)
rpPurchasedAt = Lens.field @"purchasedAt"
{-# DEPRECATED rpPurchasedAt "Use generic-lens or generic-optics with 'purchasedAt' instead." #-}

-- | Specifies whether the term of your reserved queue pricing plan is automatically extended (AUTO_RENEW) or expires (EXPIRE) at the end of the term.
--
-- /Note:/ Consider using 'renewalType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpRenewalType :: Lens.Lens' ReservationPlan (Core.Maybe Types.RenewalType)
rpRenewalType = Lens.field @"renewalType"
{-# DEPRECATED rpRenewalType "Use generic-lens or generic-optics with 'renewalType' instead." #-}

-- | Specifies the number of reserved transcode slots (RTS) for this queue. The number of RTS determines how many jobs the queue can process in parallel; each RTS can process one job at a time. When you increase this number, you extend your existing commitment with a new 12-month commitment for a larger number of RTS. The new commitment begins when you purchase the additional capacity. You can't decrease the number of RTS in your reserved queue.
--
-- /Note:/ Consider using 'reservedSlots' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpReservedSlots :: Lens.Lens' ReservationPlan (Core.Maybe Core.Int)
rpReservedSlots = Lens.field @"reservedSlots"
{-# DEPRECATED rpReservedSlots "Use generic-lens or generic-optics with 'reservedSlots' instead." #-}

-- | Specifies whether the pricing plan for your reserved queue is ACTIVE or EXPIRED.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpStatus :: Lens.Lens' ReservationPlan (Core.Maybe Types.ReservationPlanStatus)
rpStatus = Lens.field @"status"
{-# DEPRECATED rpStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON ReservationPlan where
  parseJSON =
    Core.withObject "ReservationPlan" Core.$
      \x ->
        ReservationPlan'
          Core.<$> (x Core..:? "commitment")
          Core.<*> (x Core..:? "expiresAt")
          Core.<*> (x Core..:? "purchasedAt")
          Core.<*> (x Core..:? "renewalType")
          Core.<*> (x Core..:? "reservedSlots")
          Core.<*> (x Core..:? "status")
