{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.EvaluationResult
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Rekognition.Types.EvaluationResult
  ( EvaluationResult (..)
  -- * Smart constructor
  , mkEvaluationResult
  -- * Lenses
  , erF1Score
  , erSummary
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.Summary as Types

-- | The evaluation results for the training of a model.
--
-- /See:/ 'mkEvaluationResult' smart constructor.
data EvaluationResult = EvaluationResult'
  { f1Score :: Core.Maybe Core.Double
    -- ^ The F1 score for the evaluation of all labels. The F1 score metric evaluates the overall precision and recall performance of the model as a single value. A higher value indicates better precision and recall performance. A lower score indicates that precision, recall, or both are performing poorly. 
  , summary :: Core.Maybe Types.Summary
    -- ^ The S3 bucket that contains the training summary.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EvaluationResult' value with any optional fields omitted.
mkEvaluationResult
    :: EvaluationResult
mkEvaluationResult
  = EvaluationResult'{f1Score = Core.Nothing, summary = Core.Nothing}

-- | The F1 score for the evaluation of all labels. The F1 score metric evaluates the overall precision and recall performance of the model as a single value. A higher value indicates better precision and recall performance. A lower score indicates that precision, recall, or both are performing poorly. 
--
-- /Note:/ Consider using 'f1Score' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erF1Score :: Lens.Lens' EvaluationResult (Core.Maybe Core.Double)
erF1Score = Lens.field @"f1Score"
{-# INLINEABLE erF1Score #-}
{-# DEPRECATED f1Score "Use generic-lens or generic-optics with 'f1Score' instead"  #-}

-- | The S3 bucket that contains the training summary.
--
-- /Note:/ Consider using 'summary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
erSummary :: Lens.Lens' EvaluationResult (Core.Maybe Types.Summary)
erSummary = Lens.field @"summary"
{-# INLINEABLE erSummary #-}
{-# DEPRECATED summary "Use generic-lens or generic-optics with 'summary' instead"  #-}

instance Core.FromJSON EvaluationResult where
        parseJSON
          = Core.withObject "EvaluationResult" Core.$
              \ x ->
                EvaluationResult' Core.<$>
                  (x Core..:? "F1Score") Core.<*> x Core..:? "Summary"
