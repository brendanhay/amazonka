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
-- Module      : Amazonka.Connect.Types.EvaluationFormQuestionTypeProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.EvaluationFormQuestionTypeProperties where

import Amazonka.Connect.Types.EvaluationFormNumericQuestionProperties
import Amazonka.Connect.Types.EvaluationFormSingleSelectQuestionProperties
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about properties for a question in an evaluation form. The
-- question type properties must be either for a numeric question or a
-- single select question.
--
-- /See:/ 'newEvaluationFormQuestionTypeProperties' smart constructor.
data EvaluationFormQuestionTypeProperties = EvaluationFormQuestionTypeProperties'
  { -- | The properties of the numeric question.
    numeric :: Prelude.Maybe EvaluationFormNumericQuestionProperties,
    -- | The properties of the numeric question.
    singleSelect :: Prelude.Maybe EvaluationFormSingleSelectQuestionProperties
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluationFormQuestionTypeProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'numeric', 'evaluationFormQuestionTypeProperties_numeric' - The properties of the numeric question.
--
-- 'singleSelect', 'evaluationFormQuestionTypeProperties_singleSelect' - The properties of the numeric question.
newEvaluationFormQuestionTypeProperties ::
  EvaluationFormQuestionTypeProperties
newEvaluationFormQuestionTypeProperties =
  EvaluationFormQuestionTypeProperties'
    { numeric =
        Prelude.Nothing,
      singleSelect = Prelude.Nothing
    }

-- | The properties of the numeric question.
evaluationFormQuestionTypeProperties_numeric :: Lens.Lens' EvaluationFormQuestionTypeProperties (Prelude.Maybe EvaluationFormNumericQuestionProperties)
evaluationFormQuestionTypeProperties_numeric = Lens.lens (\EvaluationFormQuestionTypeProperties' {numeric} -> numeric) (\s@EvaluationFormQuestionTypeProperties' {} a -> s {numeric = a} :: EvaluationFormQuestionTypeProperties)

-- | The properties of the numeric question.
evaluationFormQuestionTypeProperties_singleSelect :: Lens.Lens' EvaluationFormQuestionTypeProperties (Prelude.Maybe EvaluationFormSingleSelectQuestionProperties)
evaluationFormQuestionTypeProperties_singleSelect = Lens.lens (\EvaluationFormQuestionTypeProperties' {singleSelect} -> singleSelect) (\s@EvaluationFormQuestionTypeProperties' {} a -> s {singleSelect = a} :: EvaluationFormQuestionTypeProperties)

instance
  Data.FromJSON
    EvaluationFormQuestionTypeProperties
  where
  parseJSON =
    Data.withObject
      "EvaluationFormQuestionTypeProperties"
      ( \x ->
          EvaluationFormQuestionTypeProperties'
            Prelude.<$> (x Data..:? "Numeric")
            Prelude.<*> (x Data..:? "SingleSelect")
      )

instance
  Prelude.Hashable
    EvaluationFormQuestionTypeProperties
  where
  hashWithSalt
    _salt
    EvaluationFormQuestionTypeProperties' {..} =
      _salt
        `Prelude.hashWithSalt` numeric
        `Prelude.hashWithSalt` singleSelect

instance
  Prelude.NFData
    EvaluationFormQuestionTypeProperties
  where
  rnf EvaluationFormQuestionTypeProperties' {..} =
    Prelude.rnf numeric
      `Prelude.seq` Prelude.rnf singleSelect

instance
  Data.ToJSON
    EvaluationFormQuestionTypeProperties
  where
  toJSON EvaluationFormQuestionTypeProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Numeric" Data..=) Prelude.<$> numeric,
            ("SingleSelect" Data..=) Prelude.<$> singleSelect
          ]
      )
