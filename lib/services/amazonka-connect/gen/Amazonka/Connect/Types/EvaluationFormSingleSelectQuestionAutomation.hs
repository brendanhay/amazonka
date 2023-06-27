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
-- Module      : Amazonka.Connect.Types.EvaluationFormSingleSelectQuestionAutomation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.EvaluationFormSingleSelectQuestionAutomation where

import Amazonka.Connect.Types.EvaluationFormSingleSelectQuestionAutomationOption
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the automation configuration in single select
-- questions. Automation options are evaluated in order, and the first
-- matched option is applied. If no automation option matches, and there is
-- a default option, then the default option is applied.
--
-- /See:/ 'newEvaluationFormSingleSelectQuestionAutomation' smart constructor.
data EvaluationFormSingleSelectQuestionAutomation = EvaluationFormSingleSelectQuestionAutomation'
  { -- | The identifier of the default answer option, when none of the automation
    -- options match the criteria.
    defaultOptionRefId :: Prelude.Maybe Prelude.Text,
    -- | The automation options of the single select question.
    options :: Prelude.NonEmpty EvaluationFormSingleSelectQuestionAutomationOption
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluationFormSingleSelectQuestionAutomation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultOptionRefId', 'evaluationFormSingleSelectQuestionAutomation_defaultOptionRefId' - The identifier of the default answer option, when none of the automation
-- options match the criteria.
--
-- 'options', 'evaluationFormSingleSelectQuestionAutomation_options' - The automation options of the single select question.
newEvaluationFormSingleSelectQuestionAutomation ::
  -- | 'options'
  Prelude.NonEmpty EvaluationFormSingleSelectQuestionAutomationOption ->
  EvaluationFormSingleSelectQuestionAutomation
newEvaluationFormSingleSelectQuestionAutomation
  pOptions_ =
    EvaluationFormSingleSelectQuestionAutomation'
      { defaultOptionRefId =
          Prelude.Nothing,
        options =
          Lens.coerced
            Lens.# pOptions_
      }

-- | The identifier of the default answer option, when none of the automation
-- options match the criteria.
evaluationFormSingleSelectQuestionAutomation_defaultOptionRefId :: Lens.Lens' EvaluationFormSingleSelectQuestionAutomation (Prelude.Maybe Prelude.Text)
evaluationFormSingleSelectQuestionAutomation_defaultOptionRefId = Lens.lens (\EvaluationFormSingleSelectQuestionAutomation' {defaultOptionRefId} -> defaultOptionRefId) (\s@EvaluationFormSingleSelectQuestionAutomation' {} a -> s {defaultOptionRefId = a} :: EvaluationFormSingleSelectQuestionAutomation)

-- | The automation options of the single select question.
evaluationFormSingleSelectQuestionAutomation_options :: Lens.Lens' EvaluationFormSingleSelectQuestionAutomation (Prelude.NonEmpty EvaluationFormSingleSelectQuestionAutomationOption)
evaluationFormSingleSelectQuestionAutomation_options = Lens.lens (\EvaluationFormSingleSelectQuestionAutomation' {options} -> options) (\s@EvaluationFormSingleSelectQuestionAutomation' {} a -> s {options = a} :: EvaluationFormSingleSelectQuestionAutomation) Prelude.. Lens.coerced

instance
  Data.FromJSON
    EvaluationFormSingleSelectQuestionAutomation
  where
  parseJSON =
    Data.withObject
      "EvaluationFormSingleSelectQuestionAutomation"
      ( \x ->
          EvaluationFormSingleSelectQuestionAutomation'
            Prelude.<$> (x Data..:? "DefaultOptionRefId")
            Prelude.<*> (x Data..: "Options")
      )

instance
  Prelude.Hashable
    EvaluationFormSingleSelectQuestionAutomation
  where
  hashWithSalt
    _salt
    EvaluationFormSingleSelectQuestionAutomation' {..} =
      _salt
        `Prelude.hashWithSalt` defaultOptionRefId
        `Prelude.hashWithSalt` options

instance
  Prelude.NFData
    EvaluationFormSingleSelectQuestionAutomation
  where
  rnf EvaluationFormSingleSelectQuestionAutomation' {..} =
    Prelude.rnf defaultOptionRefId
      `Prelude.seq` Prelude.rnf options

instance
  Data.ToJSON
    EvaluationFormSingleSelectQuestionAutomation
  where
  toJSON
    EvaluationFormSingleSelectQuestionAutomation' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("DefaultOptionRefId" Data..=)
                Prelude.<$> defaultOptionRefId,
              Prelude.Just ("Options" Data..= options)
            ]
        )
