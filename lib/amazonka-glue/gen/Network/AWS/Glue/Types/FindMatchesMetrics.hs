{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.FindMatchesMetrics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.FindMatchesMetrics
  ( FindMatchesMetrics (..),

    -- * Smart constructor
    mkFindMatchesMetrics,

    -- * Lenses
    fmmAreaUnderPRCurve,
    fmmConfusionMatrix,
    fmmF1,
    fmmPrecision,
    fmmRecall,
  )
where

import qualified Network.AWS.Glue.Types.ConfusionMatrix as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The evaluation metrics for the find matches algorithm. The quality of your machine learning transform is measured by getting your transform to predict some matches and comparing the results to known matches from the same dataset. The quality metrics are based on a subset of your data, so they are not precise.
--
-- /See:/ 'mkFindMatchesMetrics' smart constructor.
data FindMatchesMetrics = FindMatchesMetrics'
  { -- | The area under the precision/recall curve (AUPRC) is a single number measuring the overall quality of the transform, that is independent of the choice made for precision vs. recall. Higher values indicate that you have a more attractive precision vs. recall tradeoff.
    --
    -- For more information, see <https://en.wikipedia.org/wiki/Precision_and_recall Precision and recall> in Wikipedia.
    areaUnderPRCurve :: Core.Maybe Core.Double,
    -- | The confusion matrix shows you what your transform is predicting accurately and what types of errors it is making.
    --
    -- For more information, see <https://en.wikipedia.org/wiki/Confusion_matrix Confusion matrix> in Wikipedia.
    confusionMatrix :: Core.Maybe Types.ConfusionMatrix,
    -- | The maximum F1 metric indicates the transform's accuracy between 0 and 1, where 1 is the best accuracy.
    --
    -- For more information, see <https://en.wikipedia.org/wiki/F1_score F1 score> in Wikipedia.
    f1 :: Core.Maybe Core.Double,
    -- | The precision metric indicates when often your transform is correct when it predicts a match. Specifically, it measures how well the transform finds true positives from the total true positives possible.
    --
    -- For more information, see <https://en.wikipedia.org/wiki/Precision_and_recall Precision and recall> in Wikipedia.
    precision :: Core.Maybe Core.Double,
    -- | The recall metric indicates that for an actual match, how often your transform predicts the match. Specifically, it measures how well the transform finds true positives from the total records in the source data.
    --
    -- For more information, see <https://en.wikipedia.org/wiki/Precision_and_recall Precision and recall> in Wikipedia.
    recall :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'FindMatchesMetrics' value with any optional fields omitted.
mkFindMatchesMetrics ::
  FindMatchesMetrics
mkFindMatchesMetrics =
  FindMatchesMetrics'
    { areaUnderPRCurve = Core.Nothing,
      confusionMatrix = Core.Nothing,
      f1 = Core.Nothing,
      precision = Core.Nothing,
      recall = Core.Nothing
    }

-- | The area under the precision/recall curve (AUPRC) is a single number measuring the overall quality of the transform, that is independent of the choice made for precision vs. recall. Higher values indicate that you have a more attractive precision vs. recall tradeoff.
--
-- For more information, see <https://en.wikipedia.org/wiki/Precision_and_recall Precision and recall> in Wikipedia.
--
-- /Note:/ Consider using 'areaUnderPRCurve' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmmAreaUnderPRCurve :: Lens.Lens' FindMatchesMetrics (Core.Maybe Core.Double)
fmmAreaUnderPRCurve = Lens.field @"areaUnderPRCurve"
{-# DEPRECATED fmmAreaUnderPRCurve "Use generic-lens or generic-optics with 'areaUnderPRCurve' instead." #-}

-- | The confusion matrix shows you what your transform is predicting accurately and what types of errors it is making.
--
-- For more information, see <https://en.wikipedia.org/wiki/Confusion_matrix Confusion matrix> in Wikipedia.
--
-- /Note:/ Consider using 'confusionMatrix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmmConfusionMatrix :: Lens.Lens' FindMatchesMetrics (Core.Maybe Types.ConfusionMatrix)
fmmConfusionMatrix = Lens.field @"confusionMatrix"
{-# DEPRECATED fmmConfusionMatrix "Use generic-lens or generic-optics with 'confusionMatrix' instead." #-}

-- | The maximum F1 metric indicates the transform's accuracy between 0 and 1, where 1 is the best accuracy.
--
-- For more information, see <https://en.wikipedia.org/wiki/F1_score F1 score> in Wikipedia.
--
-- /Note:/ Consider using 'f1' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmmF1 :: Lens.Lens' FindMatchesMetrics (Core.Maybe Core.Double)
fmmF1 = Lens.field @"f1"
{-# DEPRECATED fmmF1 "Use generic-lens or generic-optics with 'f1' instead." #-}

-- | The precision metric indicates when often your transform is correct when it predicts a match. Specifically, it measures how well the transform finds true positives from the total true positives possible.
--
-- For more information, see <https://en.wikipedia.org/wiki/Precision_and_recall Precision and recall> in Wikipedia.
--
-- /Note:/ Consider using 'precision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmmPrecision :: Lens.Lens' FindMatchesMetrics (Core.Maybe Core.Double)
fmmPrecision = Lens.field @"precision"
{-# DEPRECATED fmmPrecision "Use generic-lens or generic-optics with 'precision' instead." #-}

-- | The recall metric indicates that for an actual match, how often your transform predicts the match. Specifically, it measures how well the transform finds true positives from the total records in the source data.
--
-- For more information, see <https://en.wikipedia.org/wiki/Precision_and_recall Precision and recall> in Wikipedia.
--
-- /Note:/ Consider using 'recall' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmmRecall :: Lens.Lens' FindMatchesMetrics (Core.Maybe Core.Double)
fmmRecall = Lens.field @"recall"
{-# DEPRECATED fmmRecall "Use generic-lens or generic-optics with 'recall' instead." #-}

instance Core.FromJSON FindMatchesMetrics where
  parseJSON =
    Core.withObject "FindMatchesMetrics" Core.$
      \x ->
        FindMatchesMetrics'
          Core.<$> (x Core..:? "AreaUnderPRCurve")
          Core.<*> (x Core..:? "ConfusionMatrix")
          Core.<*> (x Core..:? "F1")
          Core.<*> (x Core..:? "Precision")
          Core.<*> (x Core..:? "Recall")
