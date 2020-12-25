{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    eApprovalRulesNotSatisfied,
    eApprovalRulesSatisfied,
    eApproved,
    eOverridden,
  )
where

import qualified Network.AWS.CodeCommit.Types.ApprovalRuleName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Returns information about the approval rules applied to a pull request and whether conditions have been met.
--
-- /See:/ 'mkEvaluation' smart constructor.
data Evaluation = Evaluation'
  { -- | The names of the approval rules that have not had their conditions met.
    approvalRulesNotSatisfied :: Core.Maybe [Types.ApprovalRuleName],
    -- | The names of the approval rules that have had their conditions met.
    approvalRulesSatisfied :: Core.Maybe [Types.ApprovalRuleName],
    -- | Whether the state of the pull request is approved.
    approved :: Core.Maybe Core.Bool,
    -- | Whether the approval rule requirements for the pull request have been overridden and no longer need to be met.
    overridden :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Evaluation' value with any optional fields omitted.
mkEvaluation ::
  Evaluation
mkEvaluation =
  Evaluation'
    { approvalRulesNotSatisfied = Core.Nothing,
      approvalRulesSatisfied = Core.Nothing,
      approved = Core.Nothing,
      overridden = Core.Nothing
    }

-- | The names of the approval rules that have not had their conditions met.
--
-- /Note:/ Consider using 'approvalRulesNotSatisfied' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eApprovalRulesNotSatisfied :: Lens.Lens' Evaluation (Core.Maybe [Types.ApprovalRuleName])
eApprovalRulesNotSatisfied = Lens.field @"approvalRulesNotSatisfied"
{-# DEPRECATED eApprovalRulesNotSatisfied "Use generic-lens or generic-optics with 'approvalRulesNotSatisfied' instead." #-}

-- | The names of the approval rules that have had their conditions met.
--
-- /Note:/ Consider using 'approvalRulesSatisfied' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eApprovalRulesSatisfied :: Lens.Lens' Evaluation (Core.Maybe [Types.ApprovalRuleName])
eApprovalRulesSatisfied = Lens.field @"approvalRulesSatisfied"
{-# DEPRECATED eApprovalRulesSatisfied "Use generic-lens or generic-optics with 'approvalRulesSatisfied' instead." #-}

-- | Whether the state of the pull request is approved.
--
-- /Note:/ Consider using 'approved' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eApproved :: Lens.Lens' Evaluation (Core.Maybe Core.Bool)
eApproved = Lens.field @"approved"
{-# DEPRECATED eApproved "Use generic-lens or generic-optics with 'approved' instead." #-}

-- | Whether the approval rule requirements for the pull request have been overridden and no longer need to be met.
--
-- /Note:/ Consider using 'overridden' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eOverridden :: Lens.Lens' Evaluation (Core.Maybe Core.Bool)
eOverridden = Lens.field @"overridden"
{-# DEPRECATED eOverridden "Use generic-lens or generic-optics with 'overridden' instead." #-}

instance Core.FromJSON Evaluation where
  parseJSON =
    Core.withObject "Evaluation" Core.$
      \x ->
        Evaluation'
          Core.<$> (x Core..:? "approvalRulesNotSatisfied")
          Core.<*> (x Core..:? "approvalRulesSatisfied")
          Core.<*> (x Core..:? "approved")
          Core.<*> (x Core..:? "overridden")
