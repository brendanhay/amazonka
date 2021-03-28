{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.ClassifierMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Comprehend.Types.ClassifierMetadata
  ( ClassifierMetadata (..)
  -- * Smart constructor
  , mkClassifierMetadata
  -- * Lenses
  , cmEvaluationMetrics
  , cmNumberOfLabels
  , cmNumberOfTestDocuments
  , cmNumberOfTrainedDocuments
  ) where

import qualified Network.AWS.Comprehend.Types.ClassifierEvaluationMetrics as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information about a document classifier.
--
-- /See:/ 'mkClassifierMetadata' smart constructor.
data ClassifierMetadata = ClassifierMetadata'
  { evaluationMetrics :: Core.Maybe Types.ClassifierEvaluationMetrics
    -- ^ Describes the result metrics for the test data associated with an documentation classifier.
  , numberOfLabels :: Core.Maybe Core.Int
    -- ^ The number of labels in the input data. 
  , numberOfTestDocuments :: Core.Maybe Core.Int
    -- ^ The number of documents in the input data that were used to test the classifier. Typically this is 10 to 20 percent of the input documents, up to 10,000 documents.
  , numberOfTrainedDocuments :: Core.Maybe Core.Int
    -- ^ The number of documents in the input data that were used to train the classifier. Typically this is 80 to 90 percent of the input documents.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ClassifierMetadata' value with any optional fields omitted.
mkClassifierMetadata
    :: ClassifierMetadata
mkClassifierMetadata
  = ClassifierMetadata'{evaluationMetrics = Core.Nothing,
                        numberOfLabels = Core.Nothing,
                        numberOfTestDocuments = Core.Nothing,
                        numberOfTrainedDocuments = Core.Nothing}

-- | Describes the result metrics for the test data associated with an documentation classifier.
--
-- /Note:/ Consider using 'evaluationMetrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmEvaluationMetrics :: Lens.Lens' ClassifierMetadata (Core.Maybe Types.ClassifierEvaluationMetrics)
cmEvaluationMetrics = Lens.field @"evaluationMetrics"
{-# INLINEABLE cmEvaluationMetrics #-}
{-# DEPRECATED evaluationMetrics "Use generic-lens or generic-optics with 'evaluationMetrics' instead"  #-}

-- | The number of labels in the input data. 
--
-- /Note:/ Consider using 'numberOfLabels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmNumberOfLabels :: Lens.Lens' ClassifierMetadata (Core.Maybe Core.Int)
cmNumberOfLabels = Lens.field @"numberOfLabels"
{-# INLINEABLE cmNumberOfLabels #-}
{-# DEPRECATED numberOfLabels "Use generic-lens or generic-optics with 'numberOfLabels' instead"  #-}

-- | The number of documents in the input data that were used to test the classifier. Typically this is 10 to 20 percent of the input documents, up to 10,000 documents.
--
-- /Note:/ Consider using 'numberOfTestDocuments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmNumberOfTestDocuments :: Lens.Lens' ClassifierMetadata (Core.Maybe Core.Int)
cmNumberOfTestDocuments = Lens.field @"numberOfTestDocuments"
{-# INLINEABLE cmNumberOfTestDocuments #-}
{-# DEPRECATED numberOfTestDocuments "Use generic-lens or generic-optics with 'numberOfTestDocuments' instead"  #-}

-- | The number of documents in the input data that were used to train the classifier. Typically this is 80 to 90 percent of the input documents.
--
-- /Note:/ Consider using 'numberOfTrainedDocuments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmNumberOfTrainedDocuments :: Lens.Lens' ClassifierMetadata (Core.Maybe Core.Int)
cmNumberOfTrainedDocuments = Lens.field @"numberOfTrainedDocuments"
{-# INLINEABLE cmNumberOfTrainedDocuments #-}
{-# DEPRECATED numberOfTrainedDocuments "Use generic-lens or generic-optics with 'numberOfTrainedDocuments' instead"  #-}

instance Core.FromJSON ClassifierMetadata where
        parseJSON
          = Core.withObject "ClassifierMetadata" Core.$
              \ x ->
                ClassifierMetadata' Core.<$>
                  (x Core..:? "EvaluationMetrics") Core.<*>
                    x Core..:? "NumberOfLabels"
                    Core.<*> x Core..:? "NumberOfTestDocuments"
                    Core.<*> x Core..:? "NumberOfTrainedDocuments"
