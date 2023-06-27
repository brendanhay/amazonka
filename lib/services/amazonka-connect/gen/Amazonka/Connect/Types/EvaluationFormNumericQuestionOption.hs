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
-- Module      : Amazonka.Connect.Types.EvaluationFormNumericQuestionOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.EvaluationFormNumericQuestionOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the option range used for scoring in numeric
-- questions.
--
-- /See:/ 'newEvaluationFormNumericQuestionOption' smart constructor.
data EvaluationFormNumericQuestionOption = EvaluationFormNumericQuestionOption'
  { -- | The flag to mark the option as automatic fail. If an automatic fail
    -- answer is provided, the overall evaluation gets a score of 0.
    automaticFail :: Prelude.Maybe Prelude.Bool,
    -- | The score assigned to answer values within the range option.
    score :: Prelude.Maybe Prelude.Natural,
    -- | The minimum answer value of the range option.
    minValue :: Prelude.Int,
    -- | The maximum answer value of the range option.
    maxValue :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluationFormNumericQuestionOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'automaticFail', 'evaluationFormNumericQuestionOption_automaticFail' - The flag to mark the option as automatic fail. If an automatic fail
-- answer is provided, the overall evaluation gets a score of 0.
--
-- 'score', 'evaluationFormNumericQuestionOption_score' - The score assigned to answer values within the range option.
--
-- 'minValue', 'evaluationFormNumericQuestionOption_minValue' - The minimum answer value of the range option.
--
-- 'maxValue', 'evaluationFormNumericQuestionOption_maxValue' - The maximum answer value of the range option.
newEvaluationFormNumericQuestionOption ::
  -- | 'minValue'
  Prelude.Int ->
  -- | 'maxValue'
  Prelude.Int ->
  EvaluationFormNumericQuestionOption
newEvaluationFormNumericQuestionOption
  pMinValue_
  pMaxValue_ =
    EvaluationFormNumericQuestionOption'
      { automaticFail =
          Prelude.Nothing,
        score = Prelude.Nothing,
        minValue = pMinValue_,
        maxValue = pMaxValue_
      }

-- | The flag to mark the option as automatic fail. If an automatic fail
-- answer is provided, the overall evaluation gets a score of 0.
evaluationFormNumericQuestionOption_automaticFail :: Lens.Lens' EvaluationFormNumericQuestionOption (Prelude.Maybe Prelude.Bool)
evaluationFormNumericQuestionOption_automaticFail = Lens.lens (\EvaluationFormNumericQuestionOption' {automaticFail} -> automaticFail) (\s@EvaluationFormNumericQuestionOption' {} a -> s {automaticFail = a} :: EvaluationFormNumericQuestionOption)

-- | The score assigned to answer values within the range option.
evaluationFormNumericQuestionOption_score :: Lens.Lens' EvaluationFormNumericQuestionOption (Prelude.Maybe Prelude.Natural)
evaluationFormNumericQuestionOption_score = Lens.lens (\EvaluationFormNumericQuestionOption' {score} -> score) (\s@EvaluationFormNumericQuestionOption' {} a -> s {score = a} :: EvaluationFormNumericQuestionOption)

-- | The minimum answer value of the range option.
evaluationFormNumericQuestionOption_minValue :: Lens.Lens' EvaluationFormNumericQuestionOption Prelude.Int
evaluationFormNumericQuestionOption_minValue = Lens.lens (\EvaluationFormNumericQuestionOption' {minValue} -> minValue) (\s@EvaluationFormNumericQuestionOption' {} a -> s {minValue = a} :: EvaluationFormNumericQuestionOption)

-- | The maximum answer value of the range option.
evaluationFormNumericQuestionOption_maxValue :: Lens.Lens' EvaluationFormNumericQuestionOption Prelude.Int
evaluationFormNumericQuestionOption_maxValue = Lens.lens (\EvaluationFormNumericQuestionOption' {maxValue} -> maxValue) (\s@EvaluationFormNumericQuestionOption' {} a -> s {maxValue = a} :: EvaluationFormNumericQuestionOption)

instance
  Data.FromJSON
    EvaluationFormNumericQuestionOption
  where
  parseJSON =
    Data.withObject
      "EvaluationFormNumericQuestionOption"
      ( \x ->
          EvaluationFormNumericQuestionOption'
            Prelude.<$> (x Data..:? "AutomaticFail")
            Prelude.<*> (x Data..:? "Score")
            Prelude.<*> (x Data..: "MinValue")
            Prelude.<*> (x Data..: "MaxValue")
      )

instance
  Prelude.Hashable
    EvaluationFormNumericQuestionOption
  where
  hashWithSalt
    _salt
    EvaluationFormNumericQuestionOption' {..} =
      _salt
        `Prelude.hashWithSalt` automaticFail
        `Prelude.hashWithSalt` score
        `Prelude.hashWithSalt` minValue
        `Prelude.hashWithSalt` maxValue

instance
  Prelude.NFData
    EvaluationFormNumericQuestionOption
  where
  rnf EvaluationFormNumericQuestionOption' {..} =
    Prelude.rnf automaticFail
      `Prelude.seq` Prelude.rnf score
      `Prelude.seq` Prelude.rnf minValue
      `Prelude.seq` Prelude.rnf maxValue

instance
  Data.ToJSON
    EvaluationFormNumericQuestionOption
  where
  toJSON EvaluationFormNumericQuestionOption' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AutomaticFail" Data..=) Prelude.<$> automaticFail,
            ("Score" Data..=) Prelude.<$> score,
            Prelude.Just ("MinValue" Data..= minValue),
            Prelude.Just ("MaxValue" Data..= maxValue)
          ]
      )
