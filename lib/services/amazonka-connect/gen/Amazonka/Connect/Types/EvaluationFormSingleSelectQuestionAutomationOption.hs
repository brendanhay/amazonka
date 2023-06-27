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
-- Module      : Amazonka.Connect.Types.EvaluationFormSingleSelectQuestionAutomationOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.EvaluationFormSingleSelectQuestionAutomationOption where

import Amazonka.Connect.Types.SingleSelectQuestionRuleCategoryAutomation
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the automation option of a single select question.
--
-- /See:/ 'newEvaluationFormSingleSelectQuestionAutomationOption' smart constructor.
data EvaluationFormSingleSelectQuestionAutomationOption = EvaluationFormSingleSelectQuestionAutomationOption'
  { -- | The automation option based on a rule category for the single select
    -- question.
    ruleCategory :: Prelude.Maybe SingleSelectQuestionRuleCategoryAutomation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluationFormSingleSelectQuestionAutomationOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleCategory', 'evaluationFormSingleSelectQuestionAutomationOption_ruleCategory' - The automation option based on a rule category for the single select
-- question.
newEvaluationFormSingleSelectQuestionAutomationOption ::
  EvaluationFormSingleSelectQuestionAutomationOption
newEvaluationFormSingleSelectQuestionAutomationOption =
  EvaluationFormSingleSelectQuestionAutomationOption'
    { ruleCategory =
        Prelude.Nothing
    }

-- | The automation option based on a rule category for the single select
-- question.
evaluationFormSingleSelectQuestionAutomationOption_ruleCategory :: Lens.Lens' EvaluationFormSingleSelectQuestionAutomationOption (Prelude.Maybe SingleSelectQuestionRuleCategoryAutomation)
evaluationFormSingleSelectQuestionAutomationOption_ruleCategory = Lens.lens (\EvaluationFormSingleSelectQuestionAutomationOption' {ruleCategory} -> ruleCategory) (\s@EvaluationFormSingleSelectQuestionAutomationOption' {} a -> s {ruleCategory = a} :: EvaluationFormSingleSelectQuestionAutomationOption)

instance
  Data.FromJSON
    EvaluationFormSingleSelectQuestionAutomationOption
  where
  parseJSON =
    Data.withObject
      "EvaluationFormSingleSelectQuestionAutomationOption"
      ( \x ->
          EvaluationFormSingleSelectQuestionAutomationOption'
            Prelude.<$> (x Data..:? "RuleCategory")
      )

instance
  Prelude.Hashable
    EvaluationFormSingleSelectQuestionAutomationOption
  where
  hashWithSalt
    _salt
    EvaluationFormSingleSelectQuestionAutomationOption' {..} =
      _salt `Prelude.hashWithSalt` ruleCategory

instance
  Prelude.NFData
    EvaluationFormSingleSelectQuestionAutomationOption
  where
  rnf
    EvaluationFormSingleSelectQuestionAutomationOption' {..} =
      Prelude.rnf ruleCategory

instance
  Data.ToJSON
    EvaluationFormSingleSelectQuestionAutomationOption
  where
  toJSON
    EvaluationFormSingleSelectQuestionAutomationOption' {..} =
      Data.object
        ( Prelude.catMaybes
            [("RuleCategory" Data..=) Prelude.<$> ruleCategory]
        )
