{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Rekognition.Types.EvaluationResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.EvaluationResult where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Rekognition.Types.Summary

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON EvaluationResult where
  parseJSON =
    Prelude.withObject
      "EvaluationResult"
      ( \x ->
          EvaluationResult'
            Prelude.<$> (x Prelude..:? "F1Score")
            Prelude.<*> (x Prelude..:? "Summary")
      )

instance Prelude.Hashable EvaluationResult

instance Prelude.NFData EvaluationResult
