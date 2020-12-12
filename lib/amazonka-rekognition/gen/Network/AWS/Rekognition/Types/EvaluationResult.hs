{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.EvaluationResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.EvaluationResult
  ( EvaluationResult (..),

    -- * Smart constructor
    mkEvaluationResult,

    -- * Lenses
    erSummary,
    erF1Score,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Rekognition.Types.Summary

-- | The evaluation results for the training of a model.
--
-- /See:/ 'mkEvaluationResult' smart constructor.
data EvaluationResult = EvaluationResult'
  { summary ::
      Lude.Maybe Summary,
    f1Score :: Lude.Maybe Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EvaluationResult' with the minimum fields required to make a request.
--
-- * 'f1Score' - The F1 score for the evaluation of all labels. The F1 score metric evaluates the overall precision and recall performance of the model as a single value. A higher value indicates better precision and recall performance. A lower score indicates that precision, recall, or both are performing poorly.
-- * 'summary' - The S3 bucket that contains the training summary.
mkEvaluationResult ::
  EvaluationResult
mkEvaluationResult =
  EvaluationResult' {summary = Lude.Nothing, f1Score = Lude.Nothing}

-- | The S3 bucket that contains the training summary.
--
-- /Note:/ Consider using 'summary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erSummary :: Lens.Lens' EvaluationResult (Lude.Maybe Summary)
erSummary = Lens.lens (summary :: EvaluationResult -> Lude.Maybe Summary) (\s a -> s {summary = a} :: EvaluationResult)
{-# DEPRECATED erSummary "Use generic-lens or generic-optics with 'summary' instead." #-}

-- | The F1 score for the evaluation of all labels. The F1 score metric evaluates the overall precision and recall performance of the model as a single value. A higher value indicates better precision and recall performance. A lower score indicates that precision, recall, or both are performing poorly.
--
-- /Note:/ Consider using 'f1Score' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erF1Score :: Lens.Lens' EvaluationResult (Lude.Maybe Lude.Double)
erF1Score = Lens.lens (f1Score :: EvaluationResult -> Lude.Maybe Lude.Double) (\s a -> s {f1Score = a} :: EvaluationResult)
{-# DEPRECATED erF1Score "Use generic-lens or generic-optics with 'f1Score' instead." #-}

instance Lude.FromJSON EvaluationResult where
  parseJSON =
    Lude.withObject
      "EvaluationResult"
      ( \x ->
          EvaluationResult'
            Lude.<$> (x Lude..:? "Summary") Lude.<*> (x Lude..:? "F1Score")
      )
