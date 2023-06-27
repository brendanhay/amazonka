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
-- Module      : Amazonka.Connect.Types.EvaluationAnswerInput
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.EvaluationAnswerInput where

import Amazonka.Connect.Types.EvaluationAnswerData
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about input answers for a contact evaluation.
--
-- /See:/ 'newEvaluationAnswerInput' smart constructor.
data EvaluationAnswerInput = EvaluationAnswerInput'
  { -- | The value for an answer in a contact evaluation.
    value :: Prelude.Maybe EvaluationAnswerData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluationAnswerInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'evaluationAnswerInput_value' - The value for an answer in a contact evaluation.
newEvaluationAnswerInput ::
  EvaluationAnswerInput
newEvaluationAnswerInput =
  EvaluationAnswerInput' {value = Prelude.Nothing}

-- | The value for an answer in a contact evaluation.
evaluationAnswerInput_value :: Lens.Lens' EvaluationAnswerInput (Prelude.Maybe EvaluationAnswerData)
evaluationAnswerInput_value = Lens.lens (\EvaluationAnswerInput' {value} -> value) (\s@EvaluationAnswerInput' {} a -> s {value = a} :: EvaluationAnswerInput)

instance Prelude.Hashable EvaluationAnswerInput where
  hashWithSalt _salt EvaluationAnswerInput' {..} =
    _salt `Prelude.hashWithSalt` value

instance Prelude.NFData EvaluationAnswerInput where
  rnf EvaluationAnswerInput' {..} = Prelude.rnf value

instance Data.ToJSON EvaluationAnswerInput where
  toJSON EvaluationAnswerInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Value" Data..=) Prelude.<$> value]
      )
