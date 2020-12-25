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
    etemF1Score,
    etemPrecision,
    etemRecall,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Detailed information about the accuracy of an entity recognizer for a specific entity type.
--
-- /See:/ 'mkEntityTypesEvaluationMetrics' smart constructor.
data EntityTypesEvaluationMetrics = EntityTypesEvaluationMetrics'
  { -- | A measure of how accurate the recognizer results are for a specific entity type in the test data. It is derived from the @Precision@ and @Recall@ values. The @F1Score@ is the harmonic average of the two scores. The highest score is 1, and the worst score is 0.
    f1Score :: Core.Maybe Core.Double,
    -- | A measure of the usefulness of the recognizer results for a specific entity type in the test data. High precision means that the recognizer returned substantially more relevant results than irrelevant ones.
    precision :: Core.Maybe Core.Double,
    -- | A measure of how complete the recognizer results are for a specific entity type in the test data. High recall means that the recognizer returned most of the relevant results.
    recall :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EntityTypesEvaluationMetrics' value with any optional fields omitted.
mkEntityTypesEvaluationMetrics ::
  EntityTypesEvaluationMetrics
mkEntityTypesEvaluationMetrics =
  EntityTypesEvaluationMetrics'
    { f1Score = Core.Nothing,
      precision = Core.Nothing,
      recall = Core.Nothing
    }

-- | A measure of how accurate the recognizer results are for a specific entity type in the test data. It is derived from the @Precision@ and @Recall@ values. The @F1Score@ is the harmonic average of the two scores. The highest score is 1, and the worst score is 0.
--
-- /Note:/ Consider using 'f1Score' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etemF1Score :: Lens.Lens' EntityTypesEvaluationMetrics (Core.Maybe Core.Double)
etemF1Score = Lens.field @"f1Score"
{-# DEPRECATED etemF1Score "Use generic-lens or generic-optics with 'f1Score' instead." #-}

-- | A measure of the usefulness of the recognizer results for a specific entity type in the test data. High precision means that the recognizer returned substantially more relevant results than irrelevant ones.
--
-- /Note:/ Consider using 'precision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etemPrecision :: Lens.Lens' EntityTypesEvaluationMetrics (Core.Maybe Core.Double)
etemPrecision = Lens.field @"precision"
{-# DEPRECATED etemPrecision "Use generic-lens or generic-optics with 'precision' instead." #-}

-- | A measure of how complete the recognizer results are for a specific entity type in the test data. High recall means that the recognizer returned most of the relevant results.
--
-- /Note:/ Consider using 'recall' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
etemRecall :: Lens.Lens' EntityTypesEvaluationMetrics (Core.Maybe Core.Double)
etemRecall = Lens.field @"recall"
{-# DEPRECATED etemRecall "Use generic-lens or generic-optics with 'recall' instead." #-}

instance Core.FromJSON EntityTypesEvaluationMetrics where
  parseJSON =
    Core.withObject "EntityTypesEvaluationMetrics" Core.$
      \x ->
        EntityTypesEvaluationMetrics'
          Core.<$> (x Core..:? "F1Score")
          Core.<*> (x Core..:? "Precision")
          Core.<*> (x Core..:? "Recall")
