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
-- Module      : Amazonka.Connect.Types.EvaluationFormSingleSelectQuestionProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.EvaluationFormSingleSelectQuestionProperties where

import Amazonka.Connect.Types.EvaluationFormSingleSelectQuestionAutomation
import Amazonka.Connect.Types.EvaluationFormSingleSelectQuestionDisplayMode
import Amazonka.Connect.Types.EvaluationFormSingleSelectQuestionOption
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the options in single select questions.
--
-- /See:/ 'newEvaluationFormSingleSelectQuestionProperties' smart constructor.
data EvaluationFormSingleSelectQuestionProperties = EvaluationFormSingleSelectQuestionProperties'
  { -- | The display mode of the single select question.
    automation :: Prelude.Maybe EvaluationFormSingleSelectQuestionAutomation,
    -- | The display mode of the single select question.
    displayAs :: Prelude.Maybe EvaluationFormSingleSelectQuestionDisplayMode,
    -- | The answer options of the single select question.
    options :: Prelude.NonEmpty EvaluationFormSingleSelectQuestionOption
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluationFormSingleSelectQuestionProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'automation', 'evaluationFormSingleSelectQuestionProperties_automation' - The display mode of the single select question.
--
-- 'displayAs', 'evaluationFormSingleSelectQuestionProperties_displayAs' - The display mode of the single select question.
--
-- 'options', 'evaluationFormSingleSelectQuestionProperties_options' - The answer options of the single select question.
newEvaluationFormSingleSelectQuestionProperties ::
  -- | 'options'
  Prelude.NonEmpty EvaluationFormSingleSelectQuestionOption ->
  EvaluationFormSingleSelectQuestionProperties
newEvaluationFormSingleSelectQuestionProperties
  pOptions_ =
    EvaluationFormSingleSelectQuestionProperties'
      { automation =
          Prelude.Nothing,
        displayAs = Prelude.Nothing,
        options =
          Lens.coerced
            Lens.# pOptions_
      }

-- | The display mode of the single select question.
evaluationFormSingleSelectQuestionProperties_automation :: Lens.Lens' EvaluationFormSingleSelectQuestionProperties (Prelude.Maybe EvaluationFormSingleSelectQuestionAutomation)
evaluationFormSingleSelectQuestionProperties_automation = Lens.lens (\EvaluationFormSingleSelectQuestionProperties' {automation} -> automation) (\s@EvaluationFormSingleSelectQuestionProperties' {} a -> s {automation = a} :: EvaluationFormSingleSelectQuestionProperties)

-- | The display mode of the single select question.
evaluationFormSingleSelectQuestionProperties_displayAs :: Lens.Lens' EvaluationFormSingleSelectQuestionProperties (Prelude.Maybe EvaluationFormSingleSelectQuestionDisplayMode)
evaluationFormSingleSelectQuestionProperties_displayAs = Lens.lens (\EvaluationFormSingleSelectQuestionProperties' {displayAs} -> displayAs) (\s@EvaluationFormSingleSelectQuestionProperties' {} a -> s {displayAs = a} :: EvaluationFormSingleSelectQuestionProperties)

-- | The answer options of the single select question.
evaluationFormSingleSelectQuestionProperties_options :: Lens.Lens' EvaluationFormSingleSelectQuestionProperties (Prelude.NonEmpty EvaluationFormSingleSelectQuestionOption)
evaluationFormSingleSelectQuestionProperties_options = Lens.lens (\EvaluationFormSingleSelectQuestionProperties' {options} -> options) (\s@EvaluationFormSingleSelectQuestionProperties' {} a -> s {options = a} :: EvaluationFormSingleSelectQuestionProperties) Prelude.. Lens.coerced

instance
  Data.FromJSON
    EvaluationFormSingleSelectQuestionProperties
  where
  parseJSON =
    Data.withObject
      "EvaluationFormSingleSelectQuestionProperties"
      ( \x ->
          EvaluationFormSingleSelectQuestionProperties'
            Prelude.<$> (x Data..:? "Automation")
            Prelude.<*> (x Data..:? "DisplayAs")
            Prelude.<*> (x Data..: "Options")
      )

instance
  Prelude.Hashable
    EvaluationFormSingleSelectQuestionProperties
  where
  hashWithSalt
    _salt
    EvaluationFormSingleSelectQuestionProperties' {..} =
      _salt
        `Prelude.hashWithSalt` automation
        `Prelude.hashWithSalt` displayAs
        `Prelude.hashWithSalt` options

instance
  Prelude.NFData
    EvaluationFormSingleSelectQuestionProperties
  where
  rnf EvaluationFormSingleSelectQuestionProperties' {..} =
    Prelude.rnf automation
      `Prelude.seq` Prelude.rnf displayAs
      `Prelude.seq` Prelude.rnf options

instance
  Data.ToJSON
    EvaluationFormSingleSelectQuestionProperties
  where
  toJSON
    EvaluationFormSingleSelectQuestionProperties' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("Automation" Data..=) Prelude.<$> automation,
              ("DisplayAs" Data..=) Prelude.<$> displayAs,
              Prelude.Just ("Options" Data..= options)
            ]
        )
