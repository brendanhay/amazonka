{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.ClassifierEvaluationMetrics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.ClassifierEvaluationMetrics
  ( ClassifierEvaluationMetrics (..),

    -- * Smart constructor
    mkClassifierEvaluationMetrics,

    -- * Lenses
    cemAccuracy,
    cemF1Score,
    cemHammingLoss,
    cemMicroF1Score,
    cemMicroPrecision,
    cemMicroRecall,
    cemPrecision,
    cemRecall,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the result metrics for the test data associated with an documentation classifier.
--
-- /See:/ 'mkClassifierEvaluationMetrics' smart constructor.
data ClassifierEvaluationMetrics = ClassifierEvaluationMetrics'
  { -- | The fraction of the labels that were correct recognized. It is computed by dividing the number of labels in the test documents that were correctly recognized by the total number of labels in the test documents.
    accuracy :: Core.Maybe Core.Double,
    -- | A measure of how accurate the classifier results are for the test data. It is derived from the @Precision@ and @Recall@ values. The @F1Score@ is the harmonic average of the two scores. The highest score is 1, and the worst score is 0.
    f1Score :: Core.Maybe Core.Double,
    -- | Indicates the fraction of labels that are incorrectly predicted. Also seen as the fraction of wrong labels compared to the total number of labels. Scores closer to zero are better.
    hammingLoss :: Core.Maybe Core.Double,
    -- | A measure of how accurate the classifier results are for the test data. It is a combination of the @Micro Precision@ and @Micro Recall@ values. The @Micro F1Score@ is the harmonic mean of the two scores. The highest score is 1, and the worst score is 0.
    microF1Score :: Core.Maybe Core.Double,
    -- | A measure of the usefulness of the recognizer results in the test data. High precision means that the recognizer returned substantially more relevant results than irrelevant ones. Unlike the Precision metric which comes from averaging the precision of all available labels, this is based on the overall score of all precision scores added together.
    microPrecision :: Core.Maybe Core.Double,
    -- | A measure of how complete the classifier results are for the test data. High recall means that the classifier returned most of the relevant results. Specifically, this indicates how many of the correct categories in the text that the model can predict. It is a percentage of correct categories in the text that can found. Instead of averaging the recall scores of all labels (as with Recall), micro Recall is based on the overall score of all recall scores added together.
    microRecall :: Core.Maybe Core.Double,
    -- | A measure of the usefulness of the classifier results in the test data. High precision means that the classifier returned substantially more relevant results than irrelevant ones.
    precision :: Core.Maybe Core.Double,
    -- | A measure of how complete the classifier results are for the test data. High recall means that the classifier returned most of the relevant results.
    recall :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ClassifierEvaluationMetrics' value with any optional fields omitted.
mkClassifierEvaluationMetrics ::
  ClassifierEvaluationMetrics
mkClassifierEvaluationMetrics =
  ClassifierEvaluationMetrics'
    { accuracy = Core.Nothing,
      f1Score = Core.Nothing,
      hammingLoss = Core.Nothing,
      microF1Score = Core.Nothing,
      microPrecision = Core.Nothing,
      microRecall = Core.Nothing,
      precision = Core.Nothing,
      recall = Core.Nothing
    }

-- | The fraction of the labels that were correct recognized. It is computed by dividing the number of labels in the test documents that were correctly recognized by the total number of labels in the test documents.
--
-- /Note:/ Consider using 'accuracy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cemAccuracy :: Lens.Lens' ClassifierEvaluationMetrics (Core.Maybe Core.Double)
cemAccuracy = Lens.field @"accuracy"
{-# DEPRECATED cemAccuracy "Use generic-lens or generic-optics with 'accuracy' instead." #-}

-- | A measure of how accurate the classifier results are for the test data. It is derived from the @Precision@ and @Recall@ values. The @F1Score@ is the harmonic average of the two scores. The highest score is 1, and the worst score is 0.
--
-- /Note:/ Consider using 'f1Score' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cemF1Score :: Lens.Lens' ClassifierEvaluationMetrics (Core.Maybe Core.Double)
cemF1Score = Lens.field @"f1Score"
{-# DEPRECATED cemF1Score "Use generic-lens or generic-optics with 'f1Score' instead." #-}

-- | Indicates the fraction of labels that are incorrectly predicted. Also seen as the fraction of wrong labels compared to the total number of labels. Scores closer to zero are better.
--
-- /Note:/ Consider using 'hammingLoss' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cemHammingLoss :: Lens.Lens' ClassifierEvaluationMetrics (Core.Maybe Core.Double)
cemHammingLoss = Lens.field @"hammingLoss"
{-# DEPRECATED cemHammingLoss "Use generic-lens or generic-optics with 'hammingLoss' instead." #-}

-- | A measure of how accurate the classifier results are for the test data. It is a combination of the @Micro Precision@ and @Micro Recall@ values. The @Micro F1Score@ is the harmonic mean of the two scores. The highest score is 1, and the worst score is 0.
--
-- /Note:/ Consider using 'microF1Score' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cemMicroF1Score :: Lens.Lens' ClassifierEvaluationMetrics (Core.Maybe Core.Double)
cemMicroF1Score = Lens.field @"microF1Score"
{-# DEPRECATED cemMicroF1Score "Use generic-lens or generic-optics with 'microF1Score' instead." #-}

-- | A measure of the usefulness of the recognizer results in the test data. High precision means that the recognizer returned substantially more relevant results than irrelevant ones. Unlike the Precision metric which comes from averaging the precision of all available labels, this is based on the overall score of all precision scores added together.
--
-- /Note:/ Consider using 'microPrecision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cemMicroPrecision :: Lens.Lens' ClassifierEvaluationMetrics (Core.Maybe Core.Double)
cemMicroPrecision = Lens.field @"microPrecision"
{-# DEPRECATED cemMicroPrecision "Use generic-lens or generic-optics with 'microPrecision' instead." #-}

-- | A measure of how complete the classifier results are for the test data. High recall means that the classifier returned most of the relevant results. Specifically, this indicates how many of the correct categories in the text that the model can predict. It is a percentage of correct categories in the text that can found. Instead of averaging the recall scores of all labels (as with Recall), micro Recall is based on the overall score of all recall scores added together.
--
-- /Note:/ Consider using 'microRecall' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cemMicroRecall :: Lens.Lens' ClassifierEvaluationMetrics (Core.Maybe Core.Double)
cemMicroRecall = Lens.field @"microRecall"
{-# DEPRECATED cemMicroRecall "Use generic-lens or generic-optics with 'microRecall' instead." #-}

-- | A measure of the usefulness of the classifier results in the test data. High precision means that the classifier returned substantially more relevant results than irrelevant ones.
--
-- /Note:/ Consider using 'precision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cemPrecision :: Lens.Lens' ClassifierEvaluationMetrics (Core.Maybe Core.Double)
cemPrecision = Lens.field @"precision"
{-# DEPRECATED cemPrecision "Use generic-lens or generic-optics with 'precision' instead." #-}

-- | A measure of how complete the classifier results are for the test data. High recall means that the classifier returned most of the relevant results.
--
-- /Note:/ Consider using 'recall' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cemRecall :: Lens.Lens' ClassifierEvaluationMetrics (Core.Maybe Core.Double)
cemRecall = Lens.field @"recall"
{-# DEPRECATED cemRecall "Use generic-lens or generic-optics with 'recall' instead." #-}

instance Core.FromJSON ClassifierEvaluationMetrics where
  parseJSON =
    Core.withObject "ClassifierEvaluationMetrics" Core.$
      \x ->
        ClassifierEvaluationMetrics'
          Core.<$> (x Core..:? "Accuracy")
          Core.<*> (x Core..:? "F1Score")
          Core.<*> (x Core..:? "HammingLoss")
          Core.<*> (x Core..:? "MicroF1Score")
          Core.<*> (x Core..:? "MicroPrecision")
          Core.<*> (x Core..:? "MicroRecall")
          Core.<*> (x Core..:? "Precision")
          Core.<*> (x Core..:? "Recall")
