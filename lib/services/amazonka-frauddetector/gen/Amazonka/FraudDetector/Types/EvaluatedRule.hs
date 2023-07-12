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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.EvaluatedRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details of the rule used for evaluating variable values.
--
-- /See:/ 'newEvaluatedRule' smart constructor.
data EvaluatedRule = EvaluatedRule'
  { -- | Indicates whether the rule was evaluated.
    evaluated :: Prelude.Maybe Prelude.Bool,
    -- | The rule expression.
    expression :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The rule expression value.
    expressionWithValues :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Indicates whether the rule matched.
    matched :: Prelude.Maybe Prelude.Bool,
    -- | The rule outcome.
    outcomes :: Prelude.Maybe [Prelude.Text],
    -- | The rule ID.
    ruleId :: Prelude.Maybe Prelude.Text,
    -- | The rule version.
    ruleVersion :: Prelude.Maybe Prelude.Text
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
-- 'evaluated', 'evaluatedRule_evaluated' - Indicates whether the rule was evaluated.
--
-- 'expression', 'evaluatedRule_expression' - The rule expression.
--
-- 'expressionWithValues', 'evaluatedRule_expressionWithValues' - The rule expression value.
--
-- 'matched', 'evaluatedRule_matched' - Indicates whether the rule matched.
--
-- 'outcomes', 'evaluatedRule_outcomes' - The rule outcome.
--
-- 'ruleId', 'evaluatedRule_ruleId' - The rule ID.
--
-- 'ruleVersion', 'evaluatedRule_ruleVersion' - The rule version.
newEvaluatedRule ::
  EvaluatedRule
newEvaluatedRule =
  EvaluatedRule'
    { evaluated = Prelude.Nothing,
      expression = Prelude.Nothing,
      expressionWithValues = Prelude.Nothing,
      matched = Prelude.Nothing,
      outcomes = Prelude.Nothing,
      ruleId = Prelude.Nothing,
      ruleVersion = Prelude.Nothing
    }

-- | Indicates whether the rule was evaluated.
evaluatedRule_evaluated :: Lens.Lens' EvaluatedRule (Prelude.Maybe Prelude.Bool)
evaluatedRule_evaluated = Lens.lens (\EvaluatedRule' {evaluated} -> evaluated) (\s@EvaluatedRule' {} a -> s {evaluated = a} :: EvaluatedRule)

-- | The rule expression.
evaluatedRule_expression :: Lens.Lens' EvaluatedRule (Prelude.Maybe Prelude.Text)
evaluatedRule_expression = Lens.lens (\EvaluatedRule' {expression} -> expression) (\s@EvaluatedRule' {} a -> s {expression = a} :: EvaluatedRule) Prelude.. Lens.mapping Data._Sensitive

-- | The rule expression value.
evaluatedRule_expressionWithValues :: Lens.Lens' EvaluatedRule (Prelude.Maybe Prelude.Text)
evaluatedRule_expressionWithValues = Lens.lens (\EvaluatedRule' {expressionWithValues} -> expressionWithValues) (\s@EvaluatedRule' {} a -> s {expressionWithValues = a} :: EvaluatedRule) Prelude.. Lens.mapping Data._Sensitive

-- | Indicates whether the rule matched.
evaluatedRule_matched :: Lens.Lens' EvaluatedRule (Prelude.Maybe Prelude.Bool)
evaluatedRule_matched = Lens.lens (\EvaluatedRule' {matched} -> matched) (\s@EvaluatedRule' {} a -> s {matched = a} :: EvaluatedRule)

-- | The rule outcome.
evaluatedRule_outcomes :: Lens.Lens' EvaluatedRule (Prelude.Maybe [Prelude.Text])
evaluatedRule_outcomes = Lens.lens (\EvaluatedRule' {outcomes} -> outcomes) (\s@EvaluatedRule' {} a -> s {outcomes = a} :: EvaluatedRule) Prelude.. Lens.mapping Lens.coerced

-- | The rule ID.
evaluatedRule_ruleId :: Lens.Lens' EvaluatedRule (Prelude.Maybe Prelude.Text)
evaluatedRule_ruleId = Lens.lens (\EvaluatedRule' {ruleId} -> ruleId) (\s@EvaluatedRule' {} a -> s {ruleId = a} :: EvaluatedRule)

-- | The rule version.
evaluatedRule_ruleVersion :: Lens.Lens' EvaluatedRule (Prelude.Maybe Prelude.Text)
evaluatedRule_ruleVersion = Lens.lens (\EvaluatedRule' {ruleVersion} -> ruleVersion) (\s@EvaluatedRule' {} a -> s {ruleVersion = a} :: EvaluatedRule)

instance Data.FromJSON EvaluatedRule where
  parseJSON =
    Data.withObject
      "EvaluatedRule"
      ( \x ->
          EvaluatedRule'
            Prelude.<$> (x Data..:? "evaluated")
            Prelude.<*> (x Data..:? "expression")
            Prelude.<*> (x Data..:? "expressionWithValues")
            Prelude.<*> (x Data..:? "matched")
            Prelude.<*> (x Data..:? "outcomes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ruleId")
            Prelude.<*> (x Data..:? "ruleVersion")
      )

instance Prelude.Hashable EvaluatedRule where
  hashWithSalt _salt EvaluatedRule' {..} =
    _salt
      `Prelude.hashWithSalt` evaluated
      `Prelude.hashWithSalt` expression
      `Prelude.hashWithSalt` expressionWithValues
      `Prelude.hashWithSalt` matched
      `Prelude.hashWithSalt` outcomes
      `Prelude.hashWithSalt` ruleId
      `Prelude.hashWithSalt` ruleVersion

instance Prelude.NFData EvaluatedRule where
  rnf EvaluatedRule' {..} =
    Prelude.rnf evaluated
      `Prelude.seq` Prelude.rnf expression
      `Prelude.seq` Prelude.rnf expressionWithValues
      `Prelude.seq` Prelude.rnf matched
      `Prelude.seq` Prelude.rnf outcomes
      `Prelude.seq` Prelude.rnf ruleId
      `Prelude.seq` Prelude.rnf ruleVersion
