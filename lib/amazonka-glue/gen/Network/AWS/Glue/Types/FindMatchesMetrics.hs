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
    fmmF1,
    fmmAreaUnderPRCurve,
    fmmRecall,
    fmmPrecision,
    fmmConfusionMatrix,
  )
where

import Network.AWS.Glue.Types.ConfusionMatrix
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The evaluation metrics for the find matches algorithm. The quality of your machine learning transform is measured by getting your transform to predict some matches and comparing the results to known matches from the same dataset. The quality metrics are based on a subset of your data, so they are not precise.
--
-- /See:/ 'mkFindMatchesMetrics' smart constructor.
data FindMatchesMetrics = FindMatchesMetrics'
  { f1 ::
      Lude.Maybe Lude.Double,
    areaUnderPRCurve :: Lude.Maybe Lude.Double,
    recall :: Lude.Maybe Lude.Double,
    precision :: Lude.Maybe Lude.Double,
    confusionMatrix :: Lude.Maybe ConfusionMatrix
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FindMatchesMetrics' with the minimum fields required to make a request.
--
-- * 'areaUnderPRCurve' - The area under the precision/recall curve (AUPRC) is a single number measuring the overall quality of the transform, that is independent of the choice made for precision vs. recall. Higher values indicate that you have a more attractive precision vs. recall tradeoff.
--
-- For more information, see <https://en.wikipedia.org/wiki/Precision_and_recall Precision and recall> in Wikipedia.
-- * 'confusionMatrix' - The confusion matrix shows you what your transform is predicting accurately and what types of errors it is making.
--
-- For more information, see <https://en.wikipedia.org/wiki/Confusion_matrix Confusion matrix> in Wikipedia.
-- * 'f1' - The maximum F1 metric indicates the transform's accuracy between 0 and 1, where 1 is the best accuracy.
--
-- For more information, see <https://en.wikipedia.org/wiki/F1_score F1 score> in Wikipedia.
-- * 'precision' - The precision metric indicates when often your transform is correct when it predicts a match. Specifically, it measures how well the transform finds true positives from the total true positives possible.
--
-- For more information, see <https://en.wikipedia.org/wiki/Precision_and_recall Precision and recall> in Wikipedia.
-- * 'recall' - The recall metric indicates that for an actual match, how often your transform predicts the match. Specifically, it measures how well the transform finds true positives from the total records in the source data.
--
-- For more information, see <https://en.wikipedia.org/wiki/Precision_and_recall Precision and recall> in Wikipedia.
mkFindMatchesMetrics ::
  FindMatchesMetrics
mkFindMatchesMetrics =
  FindMatchesMetrics'
    { f1 = Lude.Nothing,
      areaUnderPRCurve = Lude.Nothing,
      recall = Lude.Nothing,
      precision = Lude.Nothing,
      confusionMatrix = Lude.Nothing
    }

-- | The maximum F1 metric indicates the transform's accuracy between 0 and 1, where 1 is the best accuracy.
--
-- For more information, see <https://en.wikipedia.org/wiki/F1_score F1 score> in Wikipedia.
--
-- /Note:/ Consider using 'f1' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmmF1 :: Lens.Lens' FindMatchesMetrics (Lude.Maybe Lude.Double)
fmmF1 = Lens.lens (f1 :: FindMatchesMetrics -> Lude.Maybe Lude.Double) (\s a -> s {f1 = a} :: FindMatchesMetrics)
{-# DEPRECATED fmmF1 "Use generic-lens or generic-optics with 'f1' instead." #-}

-- | The area under the precision/recall curve (AUPRC) is a single number measuring the overall quality of the transform, that is independent of the choice made for precision vs. recall. Higher values indicate that you have a more attractive precision vs. recall tradeoff.
--
-- For more information, see <https://en.wikipedia.org/wiki/Precision_and_recall Precision and recall> in Wikipedia.
--
-- /Note:/ Consider using 'areaUnderPRCurve' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmmAreaUnderPRCurve :: Lens.Lens' FindMatchesMetrics (Lude.Maybe Lude.Double)
fmmAreaUnderPRCurve = Lens.lens (areaUnderPRCurve :: FindMatchesMetrics -> Lude.Maybe Lude.Double) (\s a -> s {areaUnderPRCurve = a} :: FindMatchesMetrics)
{-# DEPRECATED fmmAreaUnderPRCurve "Use generic-lens or generic-optics with 'areaUnderPRCurve' instead." #-}

-- | The recall metric indicates that for an actual match, how often your transform predicts the match. Specifically, it measures how well the transform finds true positives from the total records in the source data.
--
-- For more information, see <https://en.wikipedia.org/wiki/Precision_and_recall Precision and recall> in Wikipedia.
--
-- /Note:/ Consider using 'recall' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmmRecall :: Lens.Lens' FindMatchesMetrics (Lude.Maybe Lude.Double)
fmmRecall = Lens.lens (recall :: FindMatchesMetrics -> Lude.Maybe Lude.Double) (\s a -> s {recall = a} :: FindMatchesMetrics)
{-# DEPRECATED fmmRecall "Use generic-lens or generic-optics with 'recall' instead." #-}

-- | The precision metric indicates when often your transform is correct when it predicts a match. Specifically, it measures how well the transform finds true positives from the total true positives possible.
--
-- For more information, see <https://en.wikipedia.org/wiki/Precision_and_recall Precision and recall> in Wikipedia.
--
-- /Note:/ Consider using 'precision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmmPrecision :: Lens.Lens' FindMatchesMetrics (Lude.Maybe Lude.Double)
fmmPrecision = Lens.lens (precision :: FindMatchesMetrics -> Lude.Maybe Lude.Double) (\s a -> s {precision = a} :: FindMatchesMetrics)
{-# DEPRECATED fmmPrecision "Use generic-lens or generic-optics with 'precision' instead." #-}

-- | The confusion matrix shows you what your transform is predicting accurately and what types of errors it is making.
--
-- For more information, see <https://en.wikipedia.org/wiki/Confusion_matrix Confusion matrix> in Wikipedia.
--
-- /Note:/ Consider using 'confusionMatrix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fmmConfusionMatrix :: Lens.Lens' FindMatchesMetrics (Lude.Maybe ConfusionMatrix)
fmmConfusionMatrix = Lens.lens (confusionMatrix :: FindMatchesMetrics -> Lude.Maybe ConfusionMatrix) (\s a -> s {confusionMatrix = a} :: FindMatchesMetrics)
{-# DEPRECATED fmmConfusionMatrix "Use generic-lens or generic-optics with 'confusionMatrix' instead." #-}

instance Lude.FromJSON FindMatchesMetrics where
  parseJSON =
    Lude.withObject
      "FindMatchesMetrics"
      ( \x ->
          FindMatchesMetrics'
            Lude.<$> (x Lude..:? "F1")
            Lude.<*> (x Lude..:? "AreaUnderPRCurve")
            Lude.<*> (x Lude..:? "Recall")
            Lude.<*> (x Lude..:? "Precision")
            Lude.<*> (x Lude..:? "ConfusionMatrix")
      )
