{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EntityTypesEvaluationMetrics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntityTypesEvaluationMetrics
  ( EntityTypesEvaluationMetrics (..),

    -- * Smart constructor
    mkEntityTypesEvaluationMetrics,

    -- * Lenses
    etemRecall,
    etemPrecision,
    etemF1Score,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Detailed information about the accuracy of an entity recognizer for a specific entity type.
--
-- /See:/ 'mkEntityTypesEvaluationMetrics' smart constructor.
data EntityTypesEvaluationMetrics = EntityTypesEvaluationMetrics'
  { -- | A measure of how complete the recognizer results are for a specific entity type in the test data. High recall means that the recognizer returned most of the relevant results.
    recall :: Lude.Maybe Lude.Double,
    -- | A measure of the usefulness of the recognizer results for a specific entity type in the test data. High precision means that the recognizer returned substantially more relevant results than irrelevant ones.
    precision :: Lude.Maybe Lude.Double,
    -- | A measure of how accurate the recognizer results are for a specific entity type in the test data. It is derived from the @Precision@ and @Recall@ values. The @F1Score@ is the harmonic average of the two scores. The highest score is 1, and the worst score is 0.
    f1Score :: Lude.Maybe Lude.Double
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EntityTypesEvaluationMetrics' with the minimum fields required to make a request.
--
-- * 'recall' - A measure of how complete the recognizer results are for a specific entity type in the test data. High recall means that the recognizer returned most of the relevant results.
-- * 'precision' - A measure of the usefulness of the recognizer results for a specific entity type in the test data. High precision means that the recognizer returned substantially more relevant results than irrelevant ones.
-- * 'f1Score' - A measure of how accurate the recognizer results are for a specific entity type in the test data. It is derived from the @Precision@ and @Recall@ values. The @F1Score@ is the harmonic average of the two scores. The highest score is 1, and the worst score is 0.
mkEntityTypesEvaluationMetrics ::
  EntityTypesEvaluationMetrics
mkEntityTypesEvaluationMetrics =
  EntityTypesEvaluationMetrics'
    { recall = Lude.Nothing,
      precision = Lude.Nothing,
      f1Score = Lude.Nothing
    }

-- | A measure of how complete the recognizer results are for a specific entity type in the test data. High recall means that the recognizer returned most of the relevant results.
--
-- /Note:/ Consider using 'recall' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etemRecall :: Lens.Lens' EntityTypesEvaluationMetrics (Lude.Maybe Lude.Double)
etemRecall = Lens.lens (recall :: EntityTypesEvaluationMetrics -> Lude.Maybe Lude.Double) (\s a -> s {recall = a} :: EntityTypesEvaluationMetrics)
{-# DEPRECATED etemRecall "Use generic-lens or generic-optics with 'recall' instead." #-}

-- | A measure of the usefulness of the recognizer results for a specific entity type in the test data. High precision means that the recognizer returned substantially more relevant results than irrelevant ones.
--
-- /Note:/ Consider using 'precision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etemPrecision :: Lens.Lens' EntityTypesEvaluationMetrics (Lude.Maybe Lude.Double)
etemPrecision = Lens.lens (precision :: EntityTypesEvaluationMetrics -> Lude.Maybe Lude.Double) (\s a -> s {precision = a} :: EntityTypesEvaluationMetrics)
{-# DEPRECATED etemPrecision "Use generic-lens or generic-optics with 'precision' instead." #-}

-- | A measure of how accurate the recognizer results are for a specific entity type in the test data. It is derived from the @Precision@ and @Recall@ values. The @F1Score@ is the harmonic average of the two scores. The highest score is 1, and the worst score is 0.
--
-- /Note:/ Consider using 'f1Score' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etemF1Score :: Lens.Lens' EntityTypesEvaluationMetrics (Lude.Maybe Lude.Double)
etemF1Score = Lens.lens (f1Score :: EntityTypesEvaluationMetrics -> Lude.Maybe Lude.Double) (\s a -> s {f1Score = a} :: EntityTypesEvaluationMetrics)
{-# DEPRECATED etemF1Score "Use generic-lens or generic-optics with 'f1Score' instead." #-}

instance Lude.FromJSON EntityTypesEvaluationMetrics where
  parseJSON =
    Lude.withObject
      "EntityTypesEvaluationMetrics"
      ( \x ->
          EntityTypesEvaluationMetrics'
            Lude.<$> (x Lude..:? "Recall")
            Lude.<*> (x Lude..:? "Precision")
            Lude.<*> (x Lude..:? "F1Score")
      )
