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
-- Module      : Amazonka.Config.Types.EvaluationStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.EvaluationStatus where

import Amazonka.Config.Types.ResourceEvaluationStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Returns status details of an evaluation.
--
-- /See:/ 'newEvaluationStatus' smart constructor.
data EvaluationStatus = EvaluationStatus'
  { -- | An explanation for failed execution status.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The status of an execution. The valid values are In_Progress, Succeeded
    -- or Failed.
    status :: ResourceEvaluationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluationStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureReason', 'evaluationStatus_failureReason' - An explanation for failed execution status.
--
-- 'status', 'evaluationStatus_status' - The status of an execution. The valid values are In_Progress, Succeeded
-- or Failed.
newEvaluationStatus ::
  -- | 'status'
  ResourceEvaluationStatus ->
  EvaluationStatus
newEvaluationStatus pStatus_ =
  EvaluationStatus'
    { failureReason = Prelude.Nothing,
      status = pStatus_
    }

-- | An explanation for failed execution status.
evaluationStatus_failureReason :: Lens.Lens' EvaluationStatus (Prelude.Maybe Prelude.Text)
evaluationStatus_failureReason = Lens.lens (\EvaluationStatus' {failureReason} -> failureReason) (\s@EvaluationStatus' {} a -> s {failureReason = a} :: EvaluationStatus)

-- | The status of an execution. The valid values are In_Progress, Succeeded
-- or Failed.
evaluationStatus_status :: Lens.Lens' EvaluationStatus ResourceEvaluationStatus
evaluationStatus_status = Lens.lens (\EvaluationStatus' {status} -> status) (\s@EvaluationStatus' {} a -> s {status = a} :: EvaluationStatus)

instance Data.FromJSON EvaluationStatus where
  parseJSON =
    Data.withObject
      "EvaluationStatus"
      ( \x ->
          EvaluationStatus'
            Prelude.<$> (x Data..:? "FailureReason")
            Prelude.<*> (x Data..: "Status")
      )

instance Prelude.Hashable EvaluationStatus where
  hashWithSalt _salt EvaluationStatus' {..} =
    _salt
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` status

instance Prelude.NFData EvaluationStatus where
  rnf EvaluationStatus' {..} =
    Prelude.rnf failureReason `Prelude.seq`
      Prelude.rnf status
