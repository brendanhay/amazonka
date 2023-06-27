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
-- Module      : Amazonka.Connect.Types.EvaluationAnswerOutput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.EvaluationAnswerOutput where

import Amazonka.Connect.Types.EvaluationAnswerData
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about output answers for a contact evaluation.
--
-- /See:/ 'newEvaluationAnswerOutput' smart constructor.
data EvaluationAnswerOutput = EvaluationAnswerOutput'
  { -- | The system suggested value for an answer in a contact evaluation.
    systemSuggestedValue :: Prelude.Maybe EvaluationAnswerData,
    -- | The value for an answer in a contact evaluation.
    value :: Prelude.Maybe EvaluationAnswerData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluationAnswerOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'systemSuggestedValue', 'evaluationAnswerOutput_systemSuggestedValue' - The system suggested value for an answer in a contact evaluation.
--
-- 'value', 'evaluationAnswerOutput_value' - The value for an answer in a contact evaluation.
newEvaluationAnswerOutput ::
  EvaluationAnswerOutput
newEvaluationAnswerOutput =
  EvaluationAnswerOutput'
    { systemSuggestedValue =
        Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The system suggested value for an answer in a contact evaluation.
evaluationAnswerOutput_systemSuggestedValue :: Lens.Lens' EvaluationAnswerOutput (Prelude.Maybe EvaluationAnswerData)
evaluationAnswerOutput_systemSuggestedValue = Lens.lens (\EvaluationAnswerOutput' {systemSuggestedValue} -> systemSuggestedValue) (\s@EvaluationAnswerOutput' {} a -> s {systemSuggestedValue = a} :: EvaluationAnswerOutput)

-- | The value for an answer in a contact evaluation.
evaluationAnswerOutput_value :: Lens.Lens' EvaluationAnswerOutput (Prelude.Maybe EvaluationAnswerData)
evaluationAnswerOutput_value = Lens.lens (\EvaluationAnswerOutput' {value} -> value) (\s@EvaluationAnswerOutput' {} a -> s {value = a} :: EvaluationAnswerOutput)

instance Data.FromJSON EvaluationAnswerOutput where
  parseJSON =
    Data.withObject
      "EvaluationAnswerOutput"
      ( \x ->
          EvaluationAnswerOutput'
            Prelude.<$> (x Data..:? "SystemSuggestedValue")
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable EvaluationAnswerOutput where
  hashWithSalt _salt EvaluationAnswerOutput' {..} =
    _salt
      `Prelude.hashWithSalt` systemSuggestedValue
      `Prelude.hashWithSalt` value

instance Prelude.NFData EvaluationAnswerOutput where
  rnf EvaluationAnswerOutput' {..} =
    Prelude.rnf systemSuggestedValue
      `Prelude.seq` Prelude.rnf value
