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
-- Module      : Amazonka.FraudDetector.Types.EvaluatedRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.EvaluatedRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The details of the rule used for evaluating variable values.
--
-- /See:/ 'newEvaluatedRule' smart constructor.
data EvaluatedRule = EvaluatedRule'
  { -- | The rule version.
    ruleVersion :: Prelude.Maybe Prelude.Text,
    -- | The rule expression value.
    expressionWithValues :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | Indicates whether the rule matched.
    matched :: Prelude.Maybe Prelude.Bool,
    -- | The rule ID.
    ruleId :: Prelude.Maybe Prelude.Text,
    -- | The rule expression.
    expression :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The rule outcome.
    outcomes :: Prelude.Maybe [Prelude.Text],
    -- | Indicates whether the rule was evaluated.
    evaluated :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluatedRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleVersion', 'evaluatedRule_ruleVersion' - The rule version.
--
-- 'expressionWithValues', 'evaluatedRule_expressionWithValues' - The rule expression value.
--
-- 'matched', 'evaluatedRule_matched' - Indicates whether the rule matched.
--
-- 'ruleId', 'evaluatedRule_ruleId' - The rule ID.
--
-- 'expression', 'evaluatedRule_expression' - The rule expression.
--
-- 'outcomes', 'evaluatedRule_outcomes' - The rule outcome.
--
-- 'evaluated', 'evaluatedRule_evaluated' - Indicates whether the rule was evaluated.
newEvaluatedRule ::
  EvaluatedRule
newEvaluatedRule =
  EvaluatedRule'
    { ruleVersion = Prelude.Nothing,
      expressionWithValues = Prelude.Nothing,
      matched = Prelude.Nothing,
      ruleId = Prelude.Nothing,
      expression = Prelude.Nothing,
      outcomes = Prelude.Nothing,
      evaluated = Prelude.Nothing
    }

-- | The rule version.
evaluatedRule_ruleVersion :: Lens.Lens' EvaluatedRule (Prelude.Maybe Prelude.Text)
evaluatedRule_ruleVersion = Lens.lens (\EvaluatedRule' {ruleVersion} -> ruleVersion) (\s@EvaluatedRule' {} a -> s {ruleVersion = a} :: EvaluatedRule)

-- | The rule expression value.
evaluatedRule_expressionWithValues :: Lens.Lens' EvaluatedRule (Prelude.Maybe Prelude.Text)
evaluatedRule_expressionWithValues = Lens.lens (\EvaluatedRule' {expressionWithValues} -> expressionWithValues) (\s@EvaluatedRule' {} a -> s {expressionWithValues = a} :: EvaluatedRule) Prelude.. Lens.mapping Core._Sensitive

-- | Indicates whether the rule matched.
evaluatedRule_matched :: Lens.Lens' EvaluatedRule (Prelude.Maybe Prelude.Bool)
evaluatedRule_matched = Lens.lens (\EvaluatedRule' {matched} -> matched) (\s@EvaluatedRule' {} a -> s {matched = a} :: EvaluatedRule)

-- | The rule ID.
evaluatedRule_ruleId :: Lens.Lens' EvaluatedRule (Prelude.Maybe Prelude.Text)
evaluatedRule_ruleId = Lens.lens (\EvaluatedRule' {ruleId} -> ruleId) (\s@EvaluatedRule' {} a -> s {ruleId = a} :: EvaluatedRule)

-- | The rule expression.
evaluatedRule_expression :: Lens.Lens' EvaluatedRule (Prelude.Maybe Prelude.Text)
evaluatedRule_expression = Lens.lens (\EvaluatedRule' {expression} -> expression) (\s@EvaluatedRule' {} a -> s {expression = a} :: EvaluatedRule) Prelude.. Lens.mapping Core._Sensitive

-- | The rule outcome.
evaluatedRule_outcomes :: Lens.Lens' EvaluatedRule (Prelude.Maybe [Prelude.Text])
evaluatedRule_outcomes = Lens.lens (\EvaluatedRule' {outcomes} -> outcomes) (\s@EvaluatedRule' {} a -> s {outcomes = a} :: EvaluatedRule) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether the rule was evaluated.
evaluatedRule_evaluated :: Lens.Lens' EvaluatedRule (Prelude.Maybe Prelude.Bool)
evaluatedRule_evaluated = Lens.lens (\EvaluatedRule' {evaluated} -> evaluated) (\s@EvaluatedRule' {} a -> s {evaluated = a} :: EvaluatedRule)

instance Core.FromJSON EvaluatedRule where
  parseJSON =
    Core.withObject
      "EvaluatedRule"
      ( \x ->
          EvaluatedRule'
            Prelude.<$> (x Core..:? "ruleVersion")
            Prelude.<*> (x Core..:? "expressionWithValues")
            Prelude.<*> (x Core..:? "matched")
            Prelude.<*> (x Core..:? "ruleId")
            Prelude.<*> (x Core..:? "expression")
            Prelude.<*> (x Core..:? "outcomes" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "evaluated")
      )

instance Prelude.Hashable EvaluatedRule where
  hashWithSalt _salt EvaluatedRule' {..} =
    _salt `Prelude.hashWithSalt` ruleVersion
      `Prelude.hashWithSalt` expressionWithValues
      `Prelude.hashWithSalt` matched
      `Prelude.hashWithSalt` ruleId
      `Prelude.hashWithSalt` expression
      `Prelude.hashWithSalt` outcomes
      `Prelude.hashWithSalt` evaluated

instance Prelude.NFData EvaluatedRule where
  rnf EvaluatedRule' {..} =
    Prelude.rnf ruleVersion
      `Prelude.seq` Prelude.rnf expressionWithValues
      `Prelude.seq` Prelude.rnf matched
      `Prelude.seq` Prelude.rnf ruleId
      `Prelude.seq` Prelude.rnf expression
      `Prelude.seq` Prelude.rnf outcomes
      `Prelude.seq` Prelude.rnf evaluated
