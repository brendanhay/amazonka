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
-- Module      : Amazonka.Rekognition.Types.EvaluationResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.EvaluationResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.Summary

-- | The evaluation results for the training of a model.
--
-- /See:/ 'newEvaluationResult' smart constructor.
data EvaluationResult = EvaluationResult'
  { -- | The S3 bucket that contains the training summary.
    summary :: Prelude.Maybe Summary,
    -- | The F1 score for the evaluation of all labels. The F1 score metric
    -- evaluates the overall precision and recall performance of the model as a
    -- single value. A higher value indicates better precision and recall
    -- performance. A lower score indicates that precision, recall, or both are
    -- performing poorly.
    f1Score :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluationResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'summary', 'evaluationResult_summary' - The S3 bucket that contains the training summary.
--
-- 'f1Score', 'evaluationResult_f1Score' - The F1 score for the evaluation of all labels. The F1 score metric
-- evaluates the overall precision and recall performance of the model as a
-- single value. A higher value indicates better precision and recall
-- performance. A lower score indicates that precision, recall, or both are
-- performing poorly.
newEvaluationResult ::
  EvaluationResult
newEvaluationResult =
  EvaluationResult'
    { summary = Prelude.Nothing,
      f1Score = Prelude.Nothing
    }

-- | The S3 bucket that contains the training summary.
evaluationResult_summary :: Lens.Lens' EvaluationResult (Prelude.Maybe Summary)
evaluationResult_summary = Lens.lens (\EvaluationResult' {summary} -> summary) (\s@EvaluationResult' {} a -> s {summary = a} :: EvaluationResult)

-- | The F1 score for the evaluation of all labels. The F1 score metric
-- evaluates the overall precision and recall performance of the model as a
-- single value. A higher value indicates better precision and recall
-- performance. A lower score indicates that precision, recall, or both are
-- performing poorly.
evaluationResult_f1Score :: Lens.Lens' EvaluationResult (Prelude.Maybe Prelude.Double)
evaluationResult_f1Score = Lens.lens (\EvaluationResult' {f1Score} -> f1Score) (\s@EvaluationResult' {} a -> s {f1Score = a} :: EvaluationResult)

instance Core.FromJSON EvaluationResult where
  parseJSON =
    Core.withObject
      "EvaluationResult"
      ( \x ->
          EvaluationResult'
            Prelude.<$> (x Core..:? "Summary")
            Prelude.<*> (x Core..:? "F1Score")
      )

instance Prelude.Hashable EvaluationResult

instance Prelude.NFData EvaluationResult
