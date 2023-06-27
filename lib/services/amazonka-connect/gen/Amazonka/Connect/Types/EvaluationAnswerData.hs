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
-- Module      : Amazonka.Connect.Types.EvaluationAnswerData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.EvaluationAnswerData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about answer data for a contact evaluation. Answer data must
-- be either string, numeric, or not applicable.
--
-- /See:/ 'newEvaluationAnswerData' smart constructor.
data EvaluationAnswerData = EvaluationAnswerData'
  { -- | The flag to mark the question as not applicable.
    notApplicable :: Prelude.Maybe Prelude.Bool,
    -- | The numeric value for an answer in a contact evaluation.
    numericValue :: Prelude.Maybe Prelude.Double,
    -- | The string value for an answer in a contact evaluation.
    stringValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluationAnswerData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notApplicable', 'evaluationAnswerData_notApplicable' - The flag to mark the question as not applicable.
--
-- 'numericValue', 'evaluationAnswerData_numericValue' - The numeric value for an answer in a contact evaluation.
--
-- 'stringValue', 'evaluationAnswerData_stringValue' - The string value for an answer in a contact evaluation.
newEvaluationAnswerData ::
  EvaluationAnswerData
newEvaluationAnswerData =
  EvaluationAnswerData'
    { notApplicable =
        Prelude.Nothing,
      numericValue = Prelude.Nothing,
      stringValue = Prelude.Nothing
    }

-- | The flag to mark the question as not applicable.
evaluationAnswerData_notApplicable :: Lens.Lens' EvaluationAnswerData (Prelude.Maybe Prelude.Bool)
evaluationAnswerData_notApplicable = Lens.lens (\EvaluationAnswerData' {notApplicable} -> notApplicable) (\s@EvaluationAnswerData' {} a -> s {notApplicable = a} :: EvaluationAnswerData)

-- | The numeric value for an answer in a contact evaluation.
evaluationAnswerData_numericValue :: Lens.Lens' EvaluationAnswerData (Prelude.Maybe Prelude.Double)
evaluationAnswerData_numericValue = Lens.lens (\EvaluationAnswerData' {numericValue} -> numericValue) (\s@EvaluationAnswerData' {} a -> s {numericValue = a} :: EvaluationAnswerData)

-- | The string value for an answer in a contact evaluation.
evaluationAnswerData_stringValue :: Lens.Lens' EvaluationAnswerData (Prelude.Maybe Prelude.Text)
evaluationAnswerData_stringValue = Lens.lens (\EvaluationAnswerData' {stringValue} -> stringValue) (\s@EvaluationAnswerData' {} a -> s {stringValue = a} :: EvaluationAnswerData)

instance Data.FromJSON EvaluationAnswerData where
  parseJSON =
    Data.withObject
      "EvaluationAnswerData"
      ( \x ->
          EvaluationAnswerData'
            Prelude.<$> (x Data..:? "NotApplicable")
            Prelude.<*> (x Data..:? "NumericValue")
            Prelude.<*> (x Data..:? "StringValue")
      )

instance Prelude.Hashable EvaluationAnswerData where
  hashWithSalt _salt EvaluationAnswerData' {..} =
    _salt
      `Prelude.hashWithSalt` notApplicable
      `Prelude.hashWithSalt` numericValue
      `Prelude.hashWithSalt` stringValue

instance Prelude.NFData EvaluationAnswerData where
  rnf EvaluationAnswerData' {..} =
    Prelude.rnf notApplicable
      `Prelude.seq` Prelude.rnf numericValue
      `Prelude.seq` Prelude.rnf stringValue

instance Data.ToJSON EvaluationAnswerData where
  toJSON EvaluationAnswerData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NotApplicable" Data..=) Prelude.<$> notApplicable,
            ("NumericValue" Data..=) Prelude.<$> numericValue,
            ("StringValue" Data..=) Prelude.<$> stringValue
          ]
      )
