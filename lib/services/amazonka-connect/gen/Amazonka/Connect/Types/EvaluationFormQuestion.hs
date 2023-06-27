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
-- Module      : Amazonka.Connect.Types.EvaluationFormQuestion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.EvaluationFormQuestion where

import Amazonka.Connect.Types.EvaluationFormQuestionType
import Amazonka.Connect.Types.EvaluationFormQuestionTypeProperties
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a question from an evaluation form.
--
-- /See:/ 'newEvaluationFormQuestion' smart constructor.
data EvaluationFormQuestion = EvaluationFormQuestion'
  { -- | The instructions of the section.
    instructions :: Prelude.Maybe Prelude.Text,
    -- | The flag to enable not applicable answers to the question.
    notApplicableEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The properties of the type of question. Text questions do not have to
    -- define question type properties.
    questionTypeProperties :: Prelude.Maybe EvaluationFormQuestionTypeProperties,
    -- | The scoring weight of the section.
    weight :: Prelude.Maybe Prelude.Double,
    -- | The title of the question.
    title :: Prelude.Text,
    -- | The identifier of the question. An identifier must be unique within the
    -- evaluation form.
    refId :: Prelude.Text,
    -- | The type of the question.
    questionType :: EvaluationFormQuestionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluationFormQuestion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instructions', 'evaluationFormQuestion_instructions' - The instructions of the section.
--
-- 'notApplicableEnabled', 'evaluationFormQuestion_notApplicableEnabled' - The flag to enable not applicable answers to the question.
--
-- 'questionTypeProperties', 'evaluationFormQuestion_questionTypeProperties' - The properties of the type of question. Text questions do not have to
-- define question type properties.
--
-- 'weight', 'evaluationFormQuestion_weight' - The scoring weight of the section.
--
-- 'title', 'evaluationFormQuestion_title' - The title of the question.
--
-- 'refId', 'evaluationFormQuestion_refId' - The identifier of the question. An identifier must be unique within the
-- evaluation form.
--
-- 'questionType', 'evaluationFormQuestion_questionType' - The type of the question.
newEvaluationFormQuestion ::
  -- | 'title'
  Prelude.Text ->
  -- | 'refId'
  Prelude.Text ->
  -- | 'questionType'
  EvaluationFormQuestionType ->
  EvaluationFormQuestion
newEvaluationFormQuestion
  pTitle_
  pRefId_
  pQuestionType_ =
    EvaluationFormQuestion'
      { instructions =
          Prelude.Nothing,
        notApplicableEnabled = Prelude.Nothing,
        questionTypeProperties = Prelude.Nothing,
        weight = Prelude.Nothing,
        title = pTitle_,
        refId = pRefId_,
        questionType = pQuestionType_
      }

-- | The instructions of the section.
evaluationFormQuestion_instructions :: Lens.Lens' EvaluationFormQuestion (Prelude.Maybe Prelude.Text)
evaluationFormQuestion_instructions = Lens.lens (\EvaluationFormQuestion' {instructions} -> instructions) (\s@EvaluationFormQuestion' {} a -> s {instructions = a} :: EvaluationFormQuestion)

-- | The flag to enable not applicable answers to the question.
evaluationFormQuestion_notApplicableEnabled :: Lens.Lens' EvaluationFormQuestion (Prelude.Maybe Prelude.Bool)
evaluationFormQuestion_notApplicableEnabled = Lens.lens (\EvaluationFormQuestion' {notApplicableEnabled} -> notApplicableEnabled) (\s@EvaluationFormQuestion' {} a -> s {notApplicableEnabled = a} :: EvaluationFormQuestion)

-- | The properties of the type of question. Text questions do not have to
-- define question type properties.
evaluationFormQuestion_questionTypeProperties :: Lens.Lens' EvaluationFormQuestion (Prelude.Maybe EvaluationFormQuestionTypeProperties)
evaluationFormQuestion_questionTypeProperties = Lens.lens (\EvaluationFormQuestion' {questionTypeProperties} -> questionTypeProperties) (\s@EvaluationFormQuestion' {} a -> s {questionTypeProperties = a} :: EvaluationFormQuestion)

-- | The scoring weight of the section.
evaluationFormQuestion_weight :: Lens.Lens' EvaluationFormQuestion (Prelude.Maybe Prelude.Double)
evaluationFormQuestion_weight = Lens.lens (\EvaluationFormQuestion' {weight} -> weight) (\s@EvaluationFormQuestion' {} a -> s {weight = a} :: EvaluationFormQuestion)

-- | The title of the question.
evaluationFormQuestion_title :: Lens.Lens' EvaluationFormQuestion Prelude.Text
evaluationFormQuestion_title = Lens.lens (\EvaluationFormQuestion' {title} -> title) (\s@EvaluationFormQuestion' {} a -> s {title = a} :: EvaluationFormQuestion)

-- | The identifier of the question. An identifier must be unique within the
-- evaluation form.
evaluationFormQuestion_refId :: Lens.Lens' EvaluationFormQuestion Prelude.Text
evaluationFormQuestion_refId = Lens.lens (\EvaluationFormQuestion' {refId} -> refId) (\s@EvaluationFormQuestion' {} a -> s {refId = a} :: EvaluationFormQuestion)

-- | The type of the question.
evaluationFormQuestion_questionType :: Lens.Lens' EvaluationFormQuestion EvaluationFormQuestionType
evaluationFormQuestion_questionType = Lens.lens (\EvaluationFormQuestion' {questionType} -> questionType) (\s@EvaluationFormQuestion' {} a -> s {questionType = a} :: EvaluationFormQuestion)

instance Data.FromJSON EvaluationFormQuestion where
  parseJSON =
    Data.withObject
      "EvaluationFormQuestion"
      ( \x ->
          EvaluationFormQuestion'
            Prelude.<$> (x Data..:? "Instructions")
            Prelude.<*> (x Data..:? "NotApplicableEnabled")
            Prelude.<*> (x Data..:? "QuestionTypeProperties")
            Prelude.<*> (x Data..:? "Weight")
            Prelude.<*> (x Data..: "Title")
            Prelude.<*> (x Data..: "RefId")
            Prelude.<*> (x Data..: "QuestionType")
      )

instance Prelude.Hashable EvaluationFormQuestion where
  hashWithSalt _salt EvaluationFormQuestion' {..} =
    _salt
      `Prelude.hashWithSalt` instructions
      `Prelude.hashWithSalt` notApplicableEnabled
      `Prelude.hashWithSalt` questionTypeProperties
      `Prelude.hashWithSalt` weight
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` refId
      `Prelude.hashWithSalt` questionType

instance Prelude.NFData EvaluationFormQuestion where
  rnf EvaluationFormQuestion' {..} =
    Prelude.rnf instructions
      `Prelude.seq` Prelude.rnf notApplicableEnabled
      `Prelude.seq` Prelude.rnf questionTypeProperties
      `Prelude.seq` Prelude.rnf weight
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf refId
      `Prelude.seq` Prelude.rnf questionType

instance Data.ToJSON EvaluationFormQuestion where
  toJSON EvaluationFormQuestion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Instructions" Data..=) Prelude.<$> instructions,
            ("NotApplicableEnabled" Data..=)
              Prelude.<$> notApplicableEnabled,
            ("QuestionTypeProperties" Data..=)
              Prelude.<$> questionTypeProperties,
            ("Weight" Data..=) Prelude.<$> weight,
            Prelude.Just ("Title" Data..= title),
            Prelude.Just ("RefId" Data..= refId),
            Prelude.Just ("QuestionType" Data..= questionType)
          ]
      )
