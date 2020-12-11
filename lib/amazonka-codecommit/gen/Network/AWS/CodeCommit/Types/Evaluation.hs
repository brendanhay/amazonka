-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeCommit.Types.Evaluation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.Evaluation
  ( Evaluation (..),

    -- * Smart constructor
    mkEvaluation,

    -- * Lenses
    eApprovalRulesSatisfied,
    eApprovalRulesNotSatisfied,
    eApproved,
    eOverridden,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Returns information about the approval rules applied to a pull request and whether conditions have been met.
--
-- /See:/ 'mkEvaluation' smart constructor.
data Evaluation = Evaluation'
  { approvalRulesSatisfied ::
      Lude.Maybe [Lude.Text],
    approvalRulesNotSatisfied :: Lude.Maybe [Lude.Text],
    approved :: Lude.Maybe Lude.Bool,
    overridden :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Evaluation' with the minimum fields required to make a request.
--
-- * 'approvalRulesNotSatisfied' - The names of the approval rules that have not had their conditions met.
-- * 'approvalRulesSatisfied' - The names of the approval rules that have had their conditions met.
-- * 'approved' - Whether the state of the pull request is approved.
-- * 'overridden' - Whether the approval rule requirements for the pull request have been overridden and no longer need to be met.
mkEvaluation ::
  Evaluation
mkEvaluation =
  Evaluation'
    { approvalRulesSatisfied = Lude.Nothing,
      approvalRulesNotSatisfied = Lude.Nothing,
      approved = Lude.Nothing,
      overridden = Lude.Nothing
    }

-- | The names of the approval rules that have had their conditions met.
--
-- /Note:/ Consider using 'approvalRulesSatisfied' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eApprovalRulesSatisfied :: Lens.Lens' Evaluation (Lude.Maybe [Lude.Text])
eApprovalRulesSatisfied = Lens.lens (approvalRulesSatisfied :: Evaluation -> Lude.Maybe [Lude.Text]) (\s a -> s {approvalRulesSatisfied = a} :: Evaluation)
{-# DEPRECATED eApprovalRulesSatisfied "Use generic-lens or generic-optics with 'approvalRulesSatisfied' instead." #-}

-- | The names of the approval rules that have not had their conditions met.
--
-- /Note:/ Consider using 'approvalRulesNotSatisfied' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eApprovalRulesNotSatisfied :: Lens.Lens' Evaluation (Lude.Maybe [Lude.Text])
eApprovalRulesNotSatisfied = Lens.lens (approvalRulesNotSatisfied :: Evaluation -> Lude.Maybe [Lude.Text]) (\s a -> s {approvalRulesNotSatisfied = a} :: Evaluation)
{-# DEPRECATED eApprovalRulesNotSatisfied "Use generic-lens or generic-optics with 'approvalRulesNotSatisfied' instead." #-}

-- | Whether the state of the pull request is approved.
--
-- /Note:/ Consider using 'approved' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eApproved :: Lens.Lens' Evaluation (Lude.Maybe Lude.Bool)
eApproved = Lens.lens (approved :: Evaluation -> Lude.Maybe Lude.Bool) (\s a -> s {approved = a} :: Evaluation)
{-# DEPRECATED eApproved "Use generic-lens or generic-optics with 'approved' instead." #-}

-- | Whether the approval rule requirements for the pull request have been overridden and no longer need to be met.
--
-- /Note:/ Consider using 'overridden' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eOverridden :: Lens.Lens' Evaluation (Lude.Maybe Lude.Bool)
eOverridden = Lens.lens (overridden :: Evaluation -> Lude.Maybe Lude.Bool) (\s a -> s {overridden = a} :: Evaluation)
{-# DEPRECATED eOverridden "Use generic-lens or generic-optics with 'overridden' instead." #-}

instance Lude.FromJSON Evaluation where
  parseJSON =
    Lude.withObject
      "Evaluation"
      ( \x ->
          Evaluation'
            Lude.<$> (x Lude..:? "approvalRulesSatisfied" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "approvalRulesNotSatisfied" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "approved")
            Lude.<*> (x Lude..:? "overridden")
      )
