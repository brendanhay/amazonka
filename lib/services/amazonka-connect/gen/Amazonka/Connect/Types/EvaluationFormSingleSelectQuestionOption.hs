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
-- Module      : Amazonka.Connect.Types.EvaluationFormSingleSelectQuestionOption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.EvaluationFormSingleSelectQuestionOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the automation configuration in single select
-- questions.
--
-- /See:/ 'newEvaluationFormSingleSelectQuestionOption' smart constructor.
data EvaluationFormSingleSelectQuestionOption = EvaluationFormSingleSelectQuestionOption'
  { -- | The flag to mark the option as automatic fail. If an automatic fail
    -- answer is provided, the overall evaluation gets a score of 0.
    automaticFail :: Prelude.Maybe Prelude.Bool,
    -- | The score assigned to the answer option.
    score :: Prelude.Maybe Prelude.Natural,
    -- | The identifier of the answer option. An identifier must be unique within
    -- the question.
    refId :: Prelude.Text,
    -- | The title of the answer option.
    text :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluationFormSingleSelectQuestionOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'automaticFail', 'evaluationFormSingleSelectQuestionOption_automaticFail' - The flag to mark the option as automatic fail. If an automatic fail
-- answer is provided, the overall evaluation gets a score of 0.
--
-- 'score', 'evaluationFormSingleSelectQuestionOption_score' - The score assigned to the answer option.
--
-- 'refId', 'evaluationFormSingleSelectQuestionOption_refId' - The identifier of the answer option. An identifier must be unique within
-- the question.
--
-- 'text', 'evaluationFormSingleSelectQuestionOption_text' - The title of the answer option.
newEvaluationFormSingleSelectQuestionOption ::
  -- | 'refId'
  Prelude.Text ->
  -- | 'text'
  Prelude.Text ->
  EvaluationFormSingleSelectQuestionOption
newEvaluationFormSingleSelectQuestionOption
  pRefId_
  pText_ =
    EvaluationFormSingleSelectQuestionOption'
      { automaticFail =
          Prelude.Nothing,
        score = Prelude.Nothing,
        refId = pRefId_,
        text = pText_
      }

-- | The flag to mark the option as automatic fail. If an automatic fail
-- answer is provided, the overall evaluation gets a score of 0.
evaluationFormSingleSelectQuestionOption_automaticFail :: Lens.Lens' EvaluationFormSingleSelectQuestionOption (Prelude.Maybe Prelude.Bool)
evaluationFormSingleSelectQuestionOption_automaticFail = Lens.lens (\EvaluationFormSingleSelectQuestionOption' {automaticFail} -> automaticFail) (\s@EvaluationFormSingleSelectQuestionOption' {} a -> s {automaticFail = a} :: EvaluationFormSingleSelectQuestionOption)

-- | The score assigned to the answer option.
evaluationFormSingleSelectQuestionOption_score :: Lens.Lens' EvaluationFormSingleSelectQuestionOption (Prelude.Maybe Prelude.Natural)
evaluationFormSingleSelectQuestionOption_score = Lens.lens (\EvaluationFormSingleSelectQuestionOption' {score} -> score) (\s@EvaluationFormSingleSelectQuestionOption' {} a -> s {score = a} :: EvaluationFormSingleSelectQuestionOption)

-- | The identifier of the answer option. An identifier must be unique within
-- the question.
evaluationFormSingleSelectQuestionOption_refId :: Lens.Lens' EvaluationFormSingleSelectQuestionOption Prelude.Text
evaluationFormSingleSelectQuestionOption_refId = Lens.lens (\EvaluationFormSingleSelectQuestionOption' {refId} -> refId) (\s@EvaluationFormSingleSelectQuestionOption' {} a -> s {refId = a} :: EvaluationFormSingleSelectQuestionOption)

-- | The title of the answer option.
evaluationFormSingleSelectQuestionOption_text :: Lens.Lens' EvaluationFormSingleSelectQuestionOption Prelude.Text
evaluationFormSingleSelectQuestionOption_text = Lens.lens (\EvaluationFormSingleSelectQuestionOption' {text} -> text) (\s@EvaluationFormSingleSelectQuestionOption' {} a -> s {text = a} :: EvaluationFormSingleSelectQuestionOption)

instance
  Data.FromJSON
    EvaluationFormSingleSelectQuestionOption
  where
  parseJSON =
    Data.withObject
      "EvaluationFormSingleSelectQuestionOption"
      ( \x ->
          EvaluationFormSingleSelectQuestionOption'
            Prelude.<$> (x Data..:? "AutomaticFail")
            Prelude.<*> (x Data..:? "Score")
            Prelude.<*> (x Data..: "RefId")
            Prelude.<*> (x Data..: "Text")
      )

instance
  Prelude.Hashable
    EvaluationFormSingleSelectQuestionOption
  where
  hashWithSalt
    _salt
    EvaluationFormSingleSelectQuestionOption' {..} =
      _salt
        `Prelude.hashWithSalt` automaticFail
        `Prelude.hashWithSalt` score
        `Prelude.hashWithSalt` refId
        `Prelude.hashWithSalt` text

instance
  Prelude.NFData
    EvaluationFormSingleSelectQuestionOption
  where
  rnf EvaluationFormSingleSelectQuestionOption' {..} =
    Prelude.rnf automaticFail
      `Prelude.seq` Prelude.rnf score
      `Prelude.seq` Prelude.rnf refId
      `Prelude.seq` Prelude.rnf text

instance
  Data.ToJSON
    EvaluationFormSingleSelectQuestionOption
  where
  toJSON EvaluationFormSingleSelectQuestionOption' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AutomaticFail" Data..=) Prelude.<$> automaticFail,
            ("Score" Data..=) Prelude.<$> score,
            Prelude.Just ("RefId" Data..= refId),
            Prelude.Just ("Text" Data..= text)
          ]
      )
