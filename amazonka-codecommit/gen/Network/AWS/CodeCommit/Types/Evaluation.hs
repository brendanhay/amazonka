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
-- Module      : Network.AWS.CodeCommit.Types.Evaluation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.Evaluation where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Returns information about the approval rules applied to a pull request
-- and whether conditions have been met.
--
-- /See:/ 'newEvaluation' smart constructor.
data Evaluation = Evaluation'
  { -- | Whether the approval rule requirements for the pull request have been
    -- overridden and no longer need to be met.
    overridden :: Core.Maybe Core.Bool,
    -- | The names of the approval rules that have had their conditions met.
    approvalRulesSatisfied :: Core.Maybe [Core.Text],
    -- | Whether the state of the pull request is approved.
    approved :: Core.Maybe Core.Bool,
    -- | The names of the approval rules that have not had their conditions met.
    approvalRulesNotSatisfied :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Evaluation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'overridden', 'evaluation_overridden' - Whether the approval rule requirements for the pull request have been
-- overridden and no longer need to be met.
--
-- 'approvalRulesSatisfied', 'evaluation_approvalRulesSatisfied' - The names of the approval rules that have had their conditions met.
--
-- 'approved', 'evaluation_approved' - Whether the state of the pull request is approved.
--
-- 'approvalRulesNotSatisfied', 'evaluation_approvalRulesNotSatisfied' - The names of the approval rules that have not had their conditions met.
newEvaluation ::
  Evaluation
newEvaluation =
  Evaluation'
    { overridden = Core.Nothing,
      approvalRulesSatisfied = Core.Nothing,
      approved = Core.Nothing,
      approvalRulesNotSatisfied = Core.Nothing
    }

-- | Whether the approval rule requirements for the pull request have been
-- overridden and no longer need to be met.
evaluation_overridden :: Lens.Lens' Evaluation (Core.Maybe Core.Bool)
evaluation_overridden = Lens.lens (\Evaluation' {overridden} -> overridden) (\s@Evaluation' {} a -> s {overridden = a} :: Evaluation)

-- | The names of the approval rules that have had their conditions met.
evaluation_approvalRulesSatisfied :: Lens.Lens' Evaluation (Core.Maybe [Core.Text])
evaluation_approvalRulesSatisfied = Lens.lens (\Evaluation' {approvalRulesSatisfied} -> approvalRulesSatisfied) (\s@Evaluation' {} a -> s {approvalRulesSatisfied = a} :: Evaluation) Core.. Lens.mapping Lens._Coerce

-- | Whether the state of the pull request is approved.
evaluation_approved :: Lens.Lens' Evaluation (Core.Maybe Core.Bool)
evaluation_approved = Lens.lens (\Evaluation' {approved} -> approved) (\s@Evaluation' {} a -> s {approved = a} :: Evaluation)

-- | The names of the approval rules that have not had their conditions met.
evaluation_approvalRulesNotSatisfied :: Lens.Lens' Evaluation (Core.Maybe [Core.Text])
evaluation_approvalRulesNotSatisfied = Lens.lens (\Evaluation' {approvalRulesNotSatisfied} -> approvalRulesNotSatisfied) (\s@Evaluation' {} a -> s {approvalRulesNotSatisfied = a} :: Evaluation) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON Evaluation where
  parseJSON =
    Core.withObject
      "Evaluation"
      ( \x ->
          Evaluation'
            Core.<$> (x Core..:? "overridden")
            Core.<*> ( x Core..:? "approvalRulesSatisfied"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "approved")
            Core.<*> ( x Core..:? "approvalRulesNotSatisfied"
                         Core..!= Core.mempty
                     )
      )

instance Core.Hashable Evaluation

instance Core.NFData Evaluation
