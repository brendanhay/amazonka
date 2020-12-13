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
    cemMicroPrecision,
    cemMicroF1Score,
    cemRecall,
    cemPrecision,
    cemMicroRecall,
    cemF1Score,
    cemHammingLoss,
    cemAccuracy,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the result metrics for the test data associated with an documentation classifier.
--
-- /See:/ 'mkClassifierEvaluationMetrics' smart constructor.
data ClassifierEvaluationMetrics = ClassifierEvaluationMetrics'
  { -- | A measure of the usefulness of the recognizer results in the test data. High precision means that the recognizer returned substantially more relevant results than irrelevant ones. Unlike the Precision metric which comes from averaging the precision of all available labels, this is based on the overall score of all precision scores added together.
    microPrecision :: Lude.Maybe Lude.Double,
    -- | A measure of how accurate the classifier results are for the test data. It is a combination of the @Micro Precision@ and @Micro Recall@ values. The @Micro F1Score@ is the harmonic mean of the two scores. The highest score is 1, and the worst score is 0.
    microF1Score :: Lude.Maybe Lude.Double,
    -- | A measure of how complete the classifier results are for the test data. High recall means that the classifier returned most of the relevant results.
    recall :: Lude.Maybe Lude.Double,
    -- | A measure of the usefulness of the classifier results in the test data. High precision means that the classifier returned substantially more relevant results than irrelevant ones.
    precision :: Lude.Maybe Lude.Double,
    -- | A measure of how complete the classifier results are for the test data. High recall means that the classifier returned most of the relevant results. Specifically, this indicates how many of the correct categories in the text that the model can predict. It is a percentage of correct categories in the text that can found. Instead of averaging the recall scores of all labels (as with Recall), micro Recall is based on the overall score of all recall scores added together.
    microRecall :: Lude.Maybe Lude.Double,
    -- | A measure of how accurate the classifier results are for the test data. It is derived from the @Precision@ and @Recall@ values. The @F1Score@ is the harmonic average of the two scores. The highest score is 1, and the worst score is 0.
    f1Score :: Lude.Maybe Lude.Double,
    -- | Indicates the fraction of labels that are incorrectly predicted. Also seen as the fraction of wrong labels compared to the total number of labels. Scores closer to zero are better.
    hammingLoss :: Lude.Maybe Lude.Double,
    -- | The fraction of the labels that were correct recognized. It is computed by dividing the number of labels in the test documents that were correctly recognized by the total number of labels in the test documents.
    accuracy :: Lude.Maybe Lude.Double
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClassifierEvaluationMetrics' with the minimum fields required to make a request.
--
-- * 'microPrecision' - A measure of the usefulness of the recognizer results in the test data. High precision means that the recognizer returned substantially more relevant results than irrelevant ones. Unlike the Precision metric which comes from averaging the precision of all available labels, this is based on the overall score of all precision scores added together.
-- * 'microF1Score' - A measure of how accurate the classifier results are for the test data. It is a combination of the @Micro Precision@ and @Micro Recall@ values. The @Micro F1Score@ is the harmonic mean of the two scores. The highest score is 1, and the worst score is 0.
-- * 'recall' - A measure of how complete the classifier results are for the test data. High recall means that the classifier returned most of the relevant results.
-- * 'precision' - A measure of the usefulness of the classifier results in the test data. High precision means that the classifier returned substantially more relevant results than irrelevant ones.
-- * 'microRecall' - A measure of how complete the classifier results are for the test data. High recall means that the classifier returned most of the relevant results. Specifically, this indicates how many of the correct categories in the text that the model can predict. It is a percentage of correct categories in the text that can found. Instead of averaging the recall scores of all labels (as with Recall), micro Recall is based on the overall score of all recall scores added together.
-- * 'f1Score' - A measure of how accurate the classifier results are for the test data. It is derived from the @Precision@ and @Recall@ values. The @F1Score@ is the harmonic average of the two scores. The highest score is 1, and the worst score is 0.
-- * 'hammingLoss' - Indicates the fraction of labels that are incorrectly predicted. Also seen as the fraction of wrong labels compared to the total number of labels. Scores closer to zero are better.
-- * 'accuracy' - The fraction of the labels that were correct recognized. It is computed by dividing the number of labels in the test documents that were correctly recognized by the total number of labels in the test documents.
mkClassifierEvaluationMetrics ::
  ClassifierEvaluationMetrics
mkClassifierEvaluationMetrics =
  ClassifierEvaluationMetrics'
    { microPrecision = Lude.Nothing,
      microF1Score = Lude.Nothing,
      recall = Lude.Nothing,
      precision = Lude.Nothing,
      microRecall = Lude.Nothing,
      f1Score = Lude.Nothing,
      hammingLoss = Lude.Nothing,
      accuracy = Lude.Nothing
    }

-- | A measure of the usefulness of the recognizer results in the test data. High precision means that the recognizer returned substantially more relevant results than irrelevant ones. Unlike the Precision metric which comes from averaging the precision of all available labels, this is based on the overall score of all precision scores added together.
--
-- /Note:/ Consider using 'microPrecision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cemMicroPrecision :: Lens.Lens' ClassifierEvaluationMetrics (Lude.Maybe Lude.Double)
cemMicroPrecision = Lens.lens (microPrecision :: ClassifierEvaluationMetrics -> Lude.Maybe Lude.Double) (\s a -> s {microPrecision = a} :: ClassifierEvaluationMetrics)
{-# DEPRECATED cemMicroPrecision "Use generic-lens or generic-optics with 'microPrecision' instead." #-}

-- | A measure of how accurate the classifier results are for the test data. It is a combination of the @Micro Precision@ and @Micro Recall@ values. The @Micro F1Score@ is the harmonic mean of the two scores. The highest score is 1, and the worst score is 0.
--
-- /Note:/ Consider using 'microF1Score' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cemMicroF1Score :: Lens.Lens' ClassifierEvaluationMetrics (Lude.Maybe Lude.Double)
cemMicroF1Score = Lens.lens (microF1Score :: ClassifierEvaluationMetrics -> Lude.Maybe Lude.Double) (\s a -> s {microF1Score = a} :: ClassifierEvaluationMetrics)
{-# DEPRECATED cemMicroF1Score "Use generic-lens or generic-optics with 'microF1Score' instead." #-}

-- | A measure of how complete the classifier results are for the test data. High recall means that the classifier returned most of the relevant results.
--
-- /Note:/ Consider using 'recall' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cemRecall :: Lens.Lens' ClassifierEvaluationMetrics (Lude.Maybe Lude.Double)
cemRecall = Lens.lens (recall :: ClassifierEvaluationMetrics -> Lude.Maybe Lude.Double) (\s a -> s {recall = a} :: ClassifierEvaluationMetrics)
{-# DEPRECATED cemRecall "Use generic-lens or generic-optics with 'recall' instead." #-}

-- | A measure of the usefulness of the classifier results in the test data. High precision means that the classifier returned substantially more relevant results than irrelevant ones.
--
-- /Note:/ Consider using 'precision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cemPrecision :: Lens.Lens' ClassifierEvaluationMetrics (Lude.Maybe Lude.Double)
cemPrecision = Lens.lens (precision :: ClassifierEvaluationMetrics -> Lude.Maybe Lude.Double) (\s a -> s {precision = a} :: ClassifierEvaluationMetrics)
{-# DEPRECATED cemPrecision "Use generic-lens or generic-optics with 'precision' instead." #-}

-- | A measure of how complete the classifier results are for the test data. High recall means that the classifier returned most of the relevant results. Specifically, this indicates how many of the correct categories in the text that the model can predict. It is a percentage of correct categories in the text that can found. Instead of averaging the recall scores of all labels (as with Recall), micro Recall is based on the overall score of all recall scores added together.
--
-- /Note:/ Consider using 'microRecall' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cemMicroRecall :: Lens.Lens' ClassifierEvaluationMetrics (Lude.Maybe Lude.Double)
cemMicroRecall = Lens.lens (microRecall :: ClassifierEvaluationMetrics -> Lude.Maybe Lude.Double) (\s a -> s {microRecall = a} :: ClassifierEvaluationMetrics)
{-# DEPRECATED cemMicroRecall "Use generic-lens or generic-optics with 'microRecall' instead." #-}

-- | A measure of how accurate the classifier results are for the test data. It is derived from the @Precision@ and @Recall@ values. The @F1Score@ is the harmonic average of the two scores. The highest score is 1, and the worst score is 0.
--
-- /Note:/ Consider using 'f1Score' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cemF1Score :: Lens.Lens' ClassifierEvaluationMetrics (Lude.Maybe Lude.Double)
cemF1Score = Lens.lens (f1Score :: ClassifierEvaluationMetrics -> Lude.Maybe Lude.Double) (\s a -> s {f1Score = a} :: ClassifierEvaluationMetrics)
{-# DEPRECATED cemF1Score "Use generic-lens or generic-optics with 'f1Score' instead." #-}

-- | Indicates the fraction of labels that are incorrectly predicted. Also seen as the fraction of wrong labels compared to the total number of labels. Scores closer to zero are better.
--
-- /Note:/ Consider using 'hammingLoss' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cemHammingLoss :: Lens.Lens' ClassifierEvaluationMetrics (Lude.Maybe Lude.Double)
cemHammingLoss = Lens.lens (hammingLoss :: ClassifierEvaluationMetrics -> Lude.Maybe Lude.Double) (\s a -> s {hammingLoss = a} :: ClassifierEvaluationMetrics)
{-# DEPRECATED cemHammingLoss "Use generic-lens or generic-optics with 'hammingLoss' instead." #-}

-- | The fraction of the labels that were correct recognized. It is computed by dividing the number of labels in the test documents that were correctly recognized by the total number of labels in the test documents.
--
-- /Note:/ Consider using 'accuracy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cemAccuracy :: Lens.Lens' ClassifierEvaluationMetrics (Lude.Maybe Lude.Double)
cemAccuracy = Lens.lens (accuracy :: ClassifierEvaluationMetrics -> Lude.Maybe Lude.Double) (\s a -> s {accuracy = a} :: ClassifierEvaluationMetrics)
{-# DEPRECATED cemAccuracy "Use generic-lens or generic-optics with 'accuracy' instead." #-}

instance Lude.FromJSON ClassifierEvaluationMetrics where
  parseJSON =
    Lude.withObject
      "ClassifierEvaluationMetrics"
      ( \x ->
          ClassifierEvaluationMetrics'
            Lude.<$> (x Lude..:? "MicroPrecision")
            Lude.<*> (x Lude..:? "MicroF1Score")
            Lude.<*> (x Lude..:? "Recall")
            Lude.<*> (x Lude..:? "Precision")
            Lude.<*> (x Lude..:? "MicroRecall")
            Lude.<*> (x Lude..:? "F1Score")
            Lude.<*> (x Lude..:? "HammingLoss")
            Lude.<*> (x Lude..:? "Accuracy")
      )
