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
-- Module      : Amazonka.Connect.Types.EvaluationFormItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.EvaluationFormItem where

import Amazonka.Connect.Types.EvaluationFormQuestion
import {-# SOURCE #-} Amazonka.Connect.Types.EvaluationFormSection
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about an item from an evaluation form. The item must be
-- either a section or a question.
--
-- /See:/ 'newEvaluationFormItem' smart constructor.
data EvaluationFormItem = EvaluationFormItem'
  { -- | The information of the question.
    question :: Prelude.Maybe EvaluationFormQuestion,
    -- | The information of the section.
    section :: Prelude.Maybe EvaluationFormSection
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluationFormItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'question', 'evaluationFormItem_question' - The information of the question.
--
-- 'section', 'evaluationFormItem_section' - The information of the section.
newEvaluationFormItem ::
  EvaluationFormItem
newEvaluationFormItem =
  EvaluationFormItem'
    { question = Prelude.Nothing,
      section = Prelude.Nothing
    }

-- | The information of the question.
evaluationFormItem_question :: Lens.Lens' EvaluationFormItem (Prelude.Maybe EvaluationFormQuestion)
evaluationFormItem_question = Lens.lens (\EvaluationFormItem' {question} -> question) (\s@EvaluationFormItem' {} a -> s {question = a} :: EvaluationFormItem)

-- | The information of the section.
evaluationFormItem_section :: Lens.Lens' EvaluationFormItem (Prelude.Maybe EvaluationFormSection)
evaluationFormItem_section = Lens.lens (\EvaluationFormItem' {section} -> section) (\s@EvaluationFormItem' {} a -> s {section = a} :: EvaluationFormItem)

instance Data.FromJSON EvaluationFormItem where
  parseJSON =
    Data.withObject
      "EvaluationFormItem"
      ( \x ->
          EvaluationFormItem'
            Prelude.<$> (x Data..:? "Question")
            Prelude.<*> (x Data..:? "Section")
      )

instance Prelude.Hashable EvaluationFormItem where
  hashWithSalt _salt EvaluationFormItem' {..} =
    _salt
      `Prelude.hashWithSalt` question
      `Prelude.hashWithSalt` section

instance Prelude.NFData EvaluationFormItem where
  rnf EvaluationFormItem' {..} =
    Prelude.rnf question
      `Prelude.seq` Prelude.rnf section

instance Data.ToJSON EvaluationFormItem where
  toJSON EvaluationFormItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Question" Data..=) Prelude.<$> question,
            ("Section" Data..=) Prelude.<$> section
          ]
      )
