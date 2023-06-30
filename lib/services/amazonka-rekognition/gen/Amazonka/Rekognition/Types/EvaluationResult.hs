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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.EvaluationResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.Summary

-- | The evaluation results for the training of a model.
--
-- /See:/ 'newEvaluationResult' smart constructor.
data EvaluationResult = EvaluationResult'
  { -- | The F1 score for the evaluation of all labels. The F1 score metric
    -- evaluates the overall precision and recall performance of the model as a
    -- single value. A higher value indicates better precision and recall
    -- performance. A lower score indicates that precision, recall, or both are
    -- performing poorly.
    f1Score :: Prelude.Maybe Prelude.Double,
    -- | The S3 bucket that contains the training summary.
    summary :: Prelude.Maybe Summary
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
-- 'f1Score', 'evaluationResult_f1Score' - The F1 score for the evaluation of all labels. The F1 score metric
-- evaluates the overall precision and recall performance of the model as a
-- single value. A higher value indicates better precision and recall
-- performance. A lower score indicates that precision, recall, or both are
-- performing poorly.
--
-- 'summary', 'evaluationResult_summary' - The S3 bucket that contains the training summary.
newEvaluationResult ::
  EvaluationResult
newEvaluationResult =
  EvaluationResult'
    { f1Score = Prelude.Nothing,
      summary = Prelude.Nothing
    }

-- | The F1 score for the evaluation of all labels. The F1 score metric
-- evaluates the overall precision and recall performance of the model as a
-- single value. A higher value indicates better precision and recall
-- performance. A lower score indicates that precision, recall, or both are
-- performing poorly.
evaluationResult_f1Score :: Lens.Lens' EvaluationResult (Prelude.Maybe Prelude.Double)
evaluationResult_f1Score = Lens.lens (\EvaluationResult' {f1Score} -> f1Score) (\s@EvaluationResult' {} a -> s {f1Score = a} :: EvaluationResult)

-- | The S3 bucket that contains the training summary.
evaluationResult_summary :: Lens.Lens' EvaluationResult (Prelude.Maybe Summary)
evaluationResult_summary = Lens.lens (\EvaluationResult' {summary} -> summary) (\s@EvaluationResult' {} a -> s {summary = a} :: EvaluationResult)

instance Data.FromJSON EvaluationResult where
  parseJSON =
    Data.withObject
      "EvaluationResult"
      ( \x ->
          EvaluationResult'
            Prelude.<$> (x Data..:? "F1Score")
            Prelude.<*> (x Data..:? "Summary")
      )

instance Prelude.Hashable EvaluationResult where
  hashWithSalt _salt EvaluationResult' {..} =
    _salt
      `Prelude.hashWithSalt` f1Score
      `Prelude.hashWithSalt` summary

instance Prelude.NFData EvaluationResult where
  rnf EvaluationResult' {..} =
    Prelude.rnf f1Score
      `Prelude.seq` Prelude.rnf summary
