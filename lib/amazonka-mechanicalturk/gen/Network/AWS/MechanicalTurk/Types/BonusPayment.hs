{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.Types.BonusPayment
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MechanicalTurk.Types.BonusPayment
  ( BonusPayment (..),

    -- * Smart constructor
    mkBonusPayment,

    -- * Lenses
    bpAssignmentId,
    bpBonusAmount,
    bpGrantTime,
    bpReason,
    bpWorkerId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types.AssignmentId as Types
import qualified Network.AWS.MechanicalTurk.Types.BonusAmount as Types
import qualified Network.AWS.MechanicalTurk.Types.Reason as Types
import qualified Network.AWS.MechanicalTurk.Types.WorkerId as Types
import qualified Network.AWS.Prelude as Core

-- | An object representing a Bonus payment paid to a Worker.
--
-- /See:/ 'mkBonusPayment' smart constructor.
data BonusPayment = BonusPayment'
  { -- | The ID of the assignment associated with this bonus payment.
    assignmentId :: Core.Maybe Types.AssignmentId,
    bonusAmount :: Core.Maybe Types.BonusAmount,
    -- | The date and time of when the bonus was granted.
    grantTime :: Core.Maybe Core.NominalDiffTime,
    -- | The Reason text given when the bonus was granted, if any.
    reason :: Core.Maybe Types.Reason,
    -- | The ID of the Worker to whom the bonus was paid.
    workerId :: Core.Maybe Types.WorkerId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'BonusPayment' value with any optional fields omitted.
mkBonusPayment ::
  BonusPayment
mkBonusPayment =
  BonusPayment'
    { assignmentId = Core.Nothing,
      bonusAmount = Core.Nothing,
      grantTime = Core.Nothing,
      reason = Core.Nothing,
      workerId = Core.Nothing
    }

-- | The ID of the assignment associated with this bonus payment.
--
-- /Note:/ Consider using 'assignmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpAssignmentId :: Lens.Lens' BonusPayment (Core.Maybe Types.AssignmentId)
bpAssignmentId = Lens.field @"assignmentId"
{-# DEPRECATED bpAssignmentId "Use generic-lens or generic-optics with 'assignmentId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'bonusAmount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpBonusAmount :: Lens.Lens' BonusPayment (Core.Maybe Types.BonusAmount)
bpBonusAmount = Lens.field @"bonusAmount"
{-# DEPRECATED bpBonusAmount "Use generic-lens or generic-optics with 'bonusAmount' instead." #-}

-- | The date and time of when the bonus was granted.
--
-- /Note:/ Consider using 'grantTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpGrantTime :: Lens.Lens' BonusPayment (Core.Maybe Core.NominalDiffTime)
bpGrantTime = Lens.field @"grantTime"
{-# DEPRECATED bpGrantTime "Use generic-lens or generic-optics with 'grantTime' instead." #-}

-- | The Reason text given when the bonus was granted, if any.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpReason :: Lens.Lens' BonusPayment (Core.Maybe Types.Reason)
bpReason = Lens.field @"reason"
{-# DEPRECATED bpReason "Use generic-lens or generic-optics with 'reason' instead." #-}

-- | The ID of the Worker to whom the bonus was paid.
--
-- /Note:/ Consider using 'workerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpWorkerId :: Lens.Lens' BonusPayment (Core.Maybe Types.WorkerId)
bpWorkerId = Lens.field @"workerId"
{-# DEPRECATED bpWorkerId "Use generic-lens or generic-optics with 'workerId' instead." #-}

instance Core.FromJSON BonusPayment where
  parseJSON =
    Core.withObject "BonusPayment" Core.$
      \x ->
        BonusPayment'
          Core.<$> (x Core..:? "AssignmentId")
          Core.<*> (x Core..:? "BonusAmount")
          Core.<*> (x Core..:? "GrantTime")
          Core.<*> (x Core..:? "Reason")
          Core.<*> (x Core..:? "WorkerId")
