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
-- Module      : Amazonka.CodeCommit.Types.Evaluation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.Evaluation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Returns information about the approval rules applied to a pull request
-- and whether conditions have been met.
--
-- /See:/ 'newEvaluation' smart constructor.
data Evaluation = Evaluation'
  { -- | The names of the approval rules that have not had their conditions met.
    approvalRulesNotSatisfied :: Prelude.Maybe [Prelude.Text],
    -- | The names of the approval rules that have had their conditions met.
    approvalRulesSatisfied :: Prelude.Maybe [Prelude.Text],
    -- | Whether the state of the pull request is approved.
    approved :: Prelude.Maybe Prelude.Bool,
    -- | Whether the approval rule requirements for the pull request have been
    -- overridden and no longer need to be met.
    overridden :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Evaluation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'approvalRulesNotSatisfied', 'evaluation_approvalRulesNotSatisfied' - The names of the approval rules that have not had their conditions met.
--
-- 'approvalRulesSatisfied', 'evaluation_approvalRulesSatisfied' - The names of the approval rules that have had their conditions met.
--
-- 'approved', 'evaluation_approved' - Whether the state of the pull request is approved.
--
-- 'overridden', 'evaluation_overridden' - Whether the approval rule requirements for the pull request have been
-- overridden and no longer need to be met.
newEvaluation ::
  Evaluation
newEvaluation =
  Evaluation'
    { approvalRulesNotSatisfied =
        Prelude.Nothing,
      approvalRulesSatisfied = Prelude.Nothing,
      approved = Prelude.Nothing,
      overridden = Prelude.Nothing
    }

-- | The names of the approval rules that have not had their conditions met.
evaluation_approvalRulesNotSatisfied :: Lens.Lens' Evaluation (Prelude.Maybe [Prelude.Text])
evaluation_approvalRulesNotSatisfied = Lens.lens (\Evaluation' {approvalRulesNotSatisfied} -> approvalRulesNotSatisfied) (\s@Evaluation' {} a -> s {approvalRulesNotSatisfied = a} :: Evaluation) Prelude.. Lens.mapping Lens.coerced

-- | The names of the approval rules that have had their conditions met.
evaluation_approvalRulesSatisfied :: Lens.Lens' Evaluation (Prelude.Maybe [Prelude.Text])
evaluation_approvalRulesSatisfied = Lens.lens (\Evaluation' {approvalRulesSatisfied} -> approvalRulesSatisfied) (\s@Evaluation' {} a -> s {approvalRulesSatisfied = a} :: Evaluation) Prelude.. Lens.mapping Lens.coerced

-- | Whether the state of the pull request is approved.
evaluation_approved :: Lens.Lens' Evaluation (Prelude.Maybe Prelude.Bool)
evaluation_approved = Lens.lens (\Evaluation' {approved} -> approved) (\s@Evaluation' {} a -> s {approved = a} :: Evaluation)

-- | Whether the approval rule requirements for the pull request have been
-- overridden and no longer need to be met.
evaluation_overridden :: Lens.Lens' Evaluation (Prelude.Maybe Prelude.Bool)
evaluation_overridden = Lens.lens (\Evaluation' {overridden} -> overridden) (\s@Evaluation' {} a -> s {overridden = a} :: Evaluation)

instance Data.FromJSON Evaluation where
  parseJSON =
    Data.withObject
      "Evaluation"
      ( \x ->
          Evaluation'
            Prelude.<$> ( x
                            Data..:? "approvalRulesNotSatisfied"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "approvalRulesSatisfied"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "approved")
            Prelude.<*> (x Data..:? "overridden")
      )

instance Prelude.Hashable Evaluation where
  hashWithSalt _salt Evaluation' {..} =
    _salt
      `Prelude.hashWithSalt` approvalRulesNotSatisfied
      `Prelude.hashWithSalt` approvalRulesSatisfied
      `Prelude.hashWithSalt` approved
      `Prelude.hashWithSalt` overridden

instance Prelude.NFData Evaluation where
  rnf Evaluation' {..} =
    Prelude.rnf approvalRulesNotSatisfied
      `Prelude.seq` Prelude.rnf approvalRulesSatisfied
      `Prelude.seq` Prelude.rnf approved
      `Prelude.seq` Prelude.rnf overridden
