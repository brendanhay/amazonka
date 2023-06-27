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
-- Module      : Amazonka.Connect.Types.EvaluationFormNumericQuestionProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.EvaluationFormNumericQuestionProperties where

import Amazonka.Connect.Types.EvaluationFormNumericQuestionAutomation
import Amazonka.Connect.Types.EvaluationFormNumericQuestionOption
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about properties for a numeric question in an evaluation
-- form.
--
-- /See:/ 'newEvaluationFormNumericQuestionProperties' smart constructor.
data EvaluationFormNumericQuestionProperties = EvaluationFormNumericQuestionProperties'
  { -- | The automation properties of the numeric question.
    automation :: Prelude.Maybe EvaluationFormNumericQuestionAutomation,
    -- | The scoring options of the numeric question.
    options :: Prelude.Maybe (Prelude.NonEmpty EvaluationFormNumericQuestionOption),
    -- | The minimum answer value.
    minValue :: Prelude.Int,
    -- | The maximum answer value.
    maxValue :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluationFormNumericQuestionProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'automation', 'evaluationFormNumericQuestionProperties_automation' - The automation properties of the numeric question.
--
-- 'options', 'evaluationFormNumericQuestionProperties_options' - The scoring options of the numeric question.
--
-- 'minValue', 'evaluationFormNumericQuestionProperties_minValue' - The minimum answer value.
--
-- 'maxValue', 'evaluationFormNumericQuestionProperties_maxValue' - The maximum answer value.
newEvaluationFormNumericQuestionProperties ::
  -- | 'minValue'
  Prelude.Int ->
  -- | 'maxValue'
  Prelude.Int ->
  EvaluationFormNumericQuestionProperties
newEvaluationFormNumericQuestionProperties
  pMinValue_
  pMaxValue_ =
    EvaluationFormNumericQuestionProperties'
      { automation =
          Prelude.Nothing,
        options = Prelude.Nothing,
        minValue = pMinValue_,
        maxValue = pMaxValue_
      }

-- | The automation properties of the numeric question.
evaluationFormNumericQuestionProperties_automation :: Lens.Lens' EvaluationFormNumericQuestionProperties (Prelude.Maybe EvaluationFormNumericQuestionAutomation)
evaluationFormNumericQuestionProperties_automation = Lens.lens (\EvaluationFormNumericQuestionProperties' {automation} -> automation) (\s@EvaluationFormNumericQuestionProperties' {} a -> s {automation = a} :: EvaluationFormNumericQuestionProperties)

-- | The scoring options of the numeric question.
evaluationFormNumericQuestionProperties_options :: Lens.Lens' EvaluationFormNumericQuestionProperties (Prelude.Maybe (Prelude.NonEmpty EvaluationFormNumericQuestionOption))
evaluationFormNumericQuestionProperties_options = Lens.lens (\EvaluationFormNumericQuestionProperties' {options} -> options) (\s@EvaluationFormNumericQuestionProperties' {} a -> s {options = a} :: EvaluationFormNumericQuestionProperties) Prelude.. Lens.mapping Lens.coerced

-- | The minimum answer value.
evaluationFormNumericQuestionProperties_minValue :: Lens.Lens' EvaluationFormNumericQuestionProperties Prelude.Int
evaluationFormNumericQuestionProperties_minValue = Lens.lens (\EvaluationFormNumericQuestionProperties' {minValue} -> minValue) (\s@EvaluationFormNumericQuestionProperties' {} a -> s {minValue = a} :: EvaluationFormNumericQuestionProperties)

-- | The maximum answer value.
evaluationFormNumericQuestionProperties_maxValue :: Lens.Lens' EvaluationFormNumericQuestionProperties Prelude.Int
evaluationFormNumericQuestionProperties_maxValue = Lens.lens (\EvaluationFormNumericQuestionProperties' {maxValue} -> maxValue) (\s@EvaluationFormNumericQuestionProperties' {} a -> s {maxValue = a} :: EvaluationFormNumericQuestionProperties)

instance
  Data.FromJSON
    EvaluationFormNumericQuestionProperties
  where
  parseJSON =
    Data.withObject
      "EvaluationFormNumericQuestionProperties"
      ( \x ->
          EvaluationFormNumericQuestionProperties'
            Prelude.<$> (x Data..:? "Automation")
            Prelude.<*> (x Data..:? "Options")
            Prelude.<*> (x Data..: "MinValue")
            Prelude.<*> (x Data..: "MaxValue")
      )

instance
  Prelude.Hashable
    EvaluationFormNumericQuestionProperties
  where
  hashWithSalt
    _salt
    EvaluationFormNumericQuestionProperties' {..} =
      _salt
        `Prelude.hashWithSalt` automation
        `Prelude.hashWithSalt` options
        `Prelude.hashWithSalt` minValue
        `Prelude.hashWithSalt` maxValue

instance
  Prelude.NFData
    EvaluationFormNumericQuestionProperties
  where
  rnf EvaluationFormNumericQuestionProperties' {..} =
    Prelude.rnf automation
      `Prelude.seq` Prelude.rnf options
      `Prelude.seq` Prelude.rnf minValue
      `Prelude.seq` Prelude.rnf maxValue

instance
  Data.ToJSON
    EvaluationFormNumericQuestionProperties
  where
  toJSON EvaluationFormNumericQuestionProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Automation" Data..=) Prelude.<$> automation,
            ("Options" Data..=) Prelude.<$> options,
            Prelude.Just ("MinValue" Data..= minValue),
            Prelude.Just ("MaxValue" Data..= maxValue)
          ]
      )
