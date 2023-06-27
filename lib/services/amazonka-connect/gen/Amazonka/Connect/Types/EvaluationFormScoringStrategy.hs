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
-- Module      : Amazonka.Connect.Types.EvaluationFormScoringStrategy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.EvaluationFormScoringStrategy where

import Amazonka.Connect.Types.EvaluationFormScoringMode
import Amazonka.Connect.Types.EvaluationFormScoringStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about scoring strategy for an evaluation form.
--
-- /See:/ 'newEvaluationFormScoringStrategy' smart constructor.
data EvaluationFormScoringStrategy = EvaluationFormScoringStrategy'
  { -- | The scoring mode of the evaluation form.
    mode :: EvaluationFormScoringMode,
    -- | The scoring status of the evaluation form.
    status :: EvaluationFormScoringStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluationFormScoringStrategy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mode', 'evaluationFormScoringStrategy_mode' - The scoring mode of the evaluation form.
--
-- 'status', 'evaluationFormScoringStrategy_status' - The scoring status of the evaluation form.
newEvaluationFormScoringStrategy ::
  -- | 'mode'
  EvaluationFormScoringMode ->
  -- | 'status'
  EvaluationFormScoringStatus ->
  EvaluationFormScoringStrategy
newEvaluationFormScoringStrategy pMode_ pStatus_ =
  EvaluationFormScoringStrategy'
    { mode = pMode_,
      status = pStatus_
    }

-- | The scoring mode of the evaluation form.
evaluationFormScoringStrategy_mode :: Lens.Lens' EvaluationFormScoringStrategy EvaluationFormScoringMode
evaluationFormScoringStrategy_mode = Lens.lens (\EvaluationFormScoringStrategy' {mode} -> mode) (\s@EvaluationFormScoringStrategy' {} a -> s {mode = a} :: EvaluationFormScoringStrategy)

-- | The scoring status of the evaluation form.
evaluationFormScoringStrategy_status :: Lens.Lens' EvaluationFormScoringStrategy EvaluationFormScoringStatus
evaluationFormScoringStrategy_status = Lens.lens (\EvaluationFormScoringStrategy' {status} -> status) (\s@EvaluationFormScoringStrategy' {} a -> s {status = a} :: EvaluationFormScoringStrategy)

instance Data.FromJSON EvaluationFormScoringStrategy where
  parseJSON =
    Data.withObject
      "EvaluationFormScoringStrategy"
      ( \x ->
          EvaluationFormScoringStrategy'
            Prelude.<$> (x Data..: "Mode")
            Prelude.<*> (x Data..: "Status")
      )

instance
  Prelude.Hashable
    EvaluationFormScoringStrategy
  where
  hashWithSalt _salt EvaluationFormScoringStrategy' {..} =
    _salt
      `Prelude.hashWithSalt` mode
      `Prelude.hashWithSalt` status

instance Prelude.NFData EvaluationFormScoringStrategy where
  rnf EvaluationFormScoringStrategy' {..} =
    Prelude.rnf mode `Prelude.seq` Prelude.rnf status

instance Data.ToJSON EvaluationFormScoringStrategy where
  toJSON EvaluationFormScoringStrategy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Mode" Data..= mode),
            Prelude.Just ("Status" Data..= status)
          ]
      )
