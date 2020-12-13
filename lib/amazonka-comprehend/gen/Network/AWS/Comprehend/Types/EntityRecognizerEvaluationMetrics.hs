{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EntityRecognizerEvaluationMetrics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntityRecognizerEvaluationMetrics
  ( EntityRecognizerEvaluationMetrics (..),

    -- * Smart constructor
    mkEntityRecognizerEvaluationMetrics,

    -- * Lenses
    eremRecall,
    eremPrecision,
    eremF1Score,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Detailed information about the accuracy of an entity recognizer.
--
-- /See:/ 'mkEntityRecognizerEvaluationMetrics' smart constructor.
data EntityRecognizerEvaluationMetrics = EntityRecognizerEvaluationMetrics'
  { -- | A measure of how complete the recognizer results are for the test data. High recall means that the recognizer returned most of the relevant results.
    recall :: Lude.Maybe Lude.Double,
    -- | A measure of the usefulness of the recognizer results in the test data. High precision means that the recognizer returned substantially more relevant results than irrelevant ones.
    precision :: Lude.Maybe Lude.Double,
    -- | A measure of how accurate the recognizer results are for the test data. It is derived from the @Precision@ and @Recall@ values. The @F1Score@ is the harmonic average of the two scores. The highest score is 1, and the worst score is 0.
    f1Score :: Lude.Maybe Lude.Double
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EntityRecognizerEvaluationMetrics' with the minimum fields required to make a request.
--
-- * 'recall' - A measure of how complete the recognizer results are for the test data. High recall means that the recognizer returned most of the relevant results.
-- * 'precision' - A measure of the usefulness of the recognizer results in the test data. High precision means that the recognizer returned substantially more relevant results than irrelevant ones.
-- * 'f1Score' - A measure of how accurate the recognizer results are for the test data. It is derived from the @Precision@ and @Recall@ values. The @F1Score@ is the harmonic average of the two scores. The highest score is 1, and the worst score is 0.
mkEntityRecognizerEvaluationMetrics ::
  EntityRecognizerEvaluationMetrics
mkEntityRecognizerEvaluationMetrics =
  EntityRecognizerEvaluationMetrics'
    { recall = Lude.Nothing,
      precision = Lude.Nothing,
      f1Score = Lude.Nothing
    }

-- | A measure of how complete the recognizer results are for the test data. High recall means that the recognizer returned most of the relevant results.
--
-- /Note:/ Consider using 'recall' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eremRecall :: Lens.Lens' EntityRecognizerEvaluationMetrics (Lude.Maybe Lude.Double)
eremRecall = Lens.lens (recall :: EntityRecognizerEvaluationMetrics -> Lude.Maybe Lude.Double) (\s a -> s {recall = a} :: EntityRecognizerEvaluationMetrics)
{-# DEPRECATED eremRecall "Use generic-lens or generic-optics with 'recall' instead." #-}

-- | A measure of the usefulness of the recognizer results in the test data. High precision means that the recognizer returned substantially more relevant results than irrelevant ones.
--
-- /Note:/ Consider using 'precision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eremPrecision :: Lens.Lens' EntityRecognizerEvaluationMetrics (Lude.Maybe Lude.Double)
eremPrecision = Lens.lens (precision :: EntityRecognizerEvaluationMetrics -> Lude.Maybe Lude.Double) (\s a -> s {precision = a} :: EntityRecognizerEvaluationMetrics)
{-# DEPRECATED eremPrecision "Use generic-lens or generic-optics with 'precision' instead." #-}

-- | A measure of how accurate the recognizer results are for the test data. It is derived from the @Precision@ and @Recall@ values. The @F1Score@ is the harmonic average of the two scores. The highest score is 1, and the worst score is 0.
--
-- /Note:/ Consider using 'f1Score' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eremF1Score :: Lens.Lens' EntityRecognizerEvaluationMetrics (Lude.Maybe Lude.Double)
eremF1Score = Lens.lens (f1Score :: EntityRecognizerEvaluationMetrics -> Lude.Maybe Lude.Double) (\s a -> s {f1Score = a} :: EntityRecognizerEvaluationMetrics)
{-# DEPRECATED eremF1Score "Use generic-lens or generic-optics with 'f1Score' instead." #-}

instance Lude.FromJSON EntityRecognizerEvaluationMetrics where
  parseJSON =
    Lude.withObject
      "EntityRecognizerEvaluationMetrics"
      ( \x ->
          EntityRecognizerEvaluationMetrics'
            Lude.<$> (x Lude..:? "Recall")
            Lude.<*> (x Lude..:? "Precision")
            Lude.<*> (x Lude..:? "F1Score")
      )
