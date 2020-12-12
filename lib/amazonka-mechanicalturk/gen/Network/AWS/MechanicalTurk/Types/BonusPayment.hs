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
    bpReason,
    bpGrantTime,
    bpWorkerId,
    bpAssignmentId,
    bpBonusAmount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object representing a Bonus payment paid to a Worker.
--
-- /See:/ 'mkBonusPayment' smart constructor.
data BonusPayment = BonusPayment'
  { reason :: Lude.Maybe Lude.Text,
    grantTime :: Lude.Maybe Lude.Timestamp,
    workerId :: Lude.Maybe Lude.Text,
    assignmentId :: Lude.Maybe Lude.Text,
    bonusAmount :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BonusPayment' with the minimum fields required to make a request.
--
-- * 'assignmentId' - The ID of the assignment associated with this bonus payment.
-- * 'bonusAmount' - Undocumented field.
-- * 'grantTime' - The date and time of when the bonus was granted.
-- * 'reason' - The Reason text given when the bonus was granted, if any.
-- * 'workerId' - The ID of the Worker to whom the bonus was paid.
mkBonusPayment ::
  BonusPayment
mkBonusPayment =
  BonusPayment'
    { reason = Lude.Nothing,
      grantTime = Lude.Nothing,
      workerId = Lude.Nothing,
      assignmentId = Lude.Nothing,
      bonusAmount = Lude.Nothing
    }

-- | The Reason text given when the bonus was granted, if any.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpReason :: Lens.Lens' BonusPayment (Lude.Maybe Lude.Text)
bpReason = Lens.lens (reason :: BonusPayment -> Lude.Maybe Lude.Text) (\s a -> s {reason = a} :: BonusPayment)
{-# DEPRECATED bpReason "Use generic-lens or generic-optics with 'reason' instead." #-}

-- | The date and time of when the bonus was granted.
--
-- /Note:/ Consider using 'grantTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpGrantTime :: Lens.Lens' BonusPayment (Lude.Maybe Lude.Timestamp)
bpGrantTime = Lens.lens (grantTime :: BonusPayment -> Lude.Maybe Lude.Timestamp) (\s a -> s {grantTime = a} :: BonusPayment)
{-# DEPRECATED bpGrantTime "Use generic-lens or generic-optics with 'grantTime' instead." #-}

-- | The ID of the Worker to whom the bonus was paid.
--
-- /Note:/ Consider using 'workerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpWorkerId :: Lens.Lens' BonusPayment (Lude.Maybe Lude.Text)
bpWorkerId = Lens.lens (workerId :: BonusPayment -> Lude.Maybe Lude.Text) (\s a -> s {workerId = a} :: BonusPayment)
{-# DEPRECATED bpWorkerId "Use generic-lens or generic-optics with 'workerId' instead." #-}

-- | The ID of the assignment associated with this bonus payment.
--
-- /Note:/ Consider using 'assignmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpAssignmentId :: Lens.Lens' BonusPayment (Lude.Maybe Lude.Text)
bpAssignmentId = Lens.lens (assignmentId :: BonusPayment -> Lude.Maybe Lude.Text) (\s a -> s {assignmentId = a} :: BonusPayment)
{-# DEPRECATED bpAssignmentId "Use generic-lens or generic-optics with 'assignmentId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'bonusAmount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpBonusAmount :: Lens.Lens' BonusPayment (Lude.Maybe Lude.Text)
bpBonusAmount = Lens.lens (bonusAmount :: BonusPayment -> Lude.Maybe Lude.Text) (\s a -> s {bonusAmount = a} :: BonusPayment)
{-# DEPRECATED bpBonusAmount "Use generic-lens or generic-optics with 'bonusAmount' instead." #-}

instance Lude.FromJSON BonusPayment where
  parseJSON =
    Lude.withObject
      "BonusPayment"
      ( \x ->
          BonusPayment'
            Lude.<$> (x Lude..:? "Reason")
            Lude.<*> (x Lude..:? "GrantTime")
            Lude.<*> (x Lude..:? "WorkerId")
            Lude.<*> (x Lude..:? "AssignmentId")
            Lude.<*> (x Lude..:? "BonusAmount")
      )
