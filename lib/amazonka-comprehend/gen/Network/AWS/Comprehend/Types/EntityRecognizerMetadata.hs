{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EntityRecognizerMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Comprehend.Types.EntityRecognizerMetadata
  ( EntityRecognizerMetadata (..)
  -- * Smart constructor
  , mkEntityRecognizerMetadata
  -- * Lenses
  , ermEntityTypes
  , ermEvaluationMetrics
  , ermNumberOfTestDocuments
  , ermNumberOfTrainedDocuments
  ) where

import qualified Network.AWS.Comprehend.Types.EntityRecognizerEvaluationMetrics as Types
import qualified Network.AWS.Comprehend.Types.EntityRecognizerMetadataEntityTypesListItem as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Detailed information about an entity recognizer.
--
-- /See:/ 'mkEntityRecognizerMetadata' smart constructor.
data EntityRecognizerMetadata = EntityRecognizerMetadata'
  { entityTypes :: Core.Maybe [Types.EntityRecognizerMetadataEntityTypesListItem]
    -- ^ Entity types from the metadata of an entity recognizer.
  , evaluationMetrics :: Core.Maybe Types.EntityRecognizerEvaluationMetrics
    -- ^ Detailed information about the accuracy of an entity recognizer.
  , numberOfTestDocuments :: Core.Maybe Core.Int
    -- ^ The number of documents in the input data that were used to test the entity recognizer. Typically this is 10 to 20 percent of the input documents.
  , numberOfTrainedDocuments :: Core.Maybe Core.Int
    -- ^ The number of documents in the input data that were used to train the entity recognizer. Typically this is 80 to 90 percent of the input documents.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EntityRecognizerMetadata' value with any optional fields omitted.
mkEntityRecognizerMetadata
    :: EntityRecognizerMetadata
mkEntityRecognizerMetadata
  = EntityRecognizerMetadata'{entityTypes = Core.Nothing,
                              evaluationMetrics = Core.Nothing,
                              numberOfTestDocuments = Core.Nothing,
                              numberOfTrainedDocuments = Core.Nothing}

-- | Entity types from the metadata of an entity recognizer.
--
-- /Note:/ Consider using 'entityTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ermEntityTypes :: Lens.Lens' EntityRecognizerMetadata (Core.Maybe [Types.EntityRecognizerMetadataEntityTypesListItem])
ermEntityTypes = Lens.field @"entityTypes"
{-# INLINEABLE ermEntityTypes #-}
{-# DEPRECATED entityTypes "Use generic-lens or generic-optics with 'entityTypes' instead"  #-}

-- | Detailed information about the accuracy of an entity recognizer.
--
-- /Note:/ Consider using 'evaluationMetrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ermEvaluationMetrics :: Lens.Lens' EntityRecognizerMetadata (Core.Maybe Types.EntityRecognizerEvaluationMetrics)
ermEvaluationMetrics = Lens.field @"evaluationMetrics"
{-# INLINEABLE ermEvaluationMetrics #-}
{-# DEPRECATED evaluationMetrics "Use generic-lens or generic-optics with 'evaluationMetrics' instead"  #-}

-- | The number of documents in the input data that were used to test the entity recognizer. Typically this is 10 to 20 percent of the input documents.
--
-- /Note:/ Consider using 'numberOfTestDocuments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ermNumberOfTestDocuments :: Lens.Lens' EntityRecognizerMetadata (Core.Maybe Core.Int)
ermNumberOfTestDocuments = Lens.field @"numberOfTestDocuments"
{-# INLINEABLE ermNumberOfTestDocuments #-}
{-# DEPRECATED numberOfTestDocuments "Use generic-lens or generic-optics with 'numberOfTestDocuments' instead"  #-}

-- | The number of documents in the input data that were used to train the entity recognizer. Typically this is 80 to 90 percent of the input documents.
--
-- /Note:/ Consider using 'numberOfTrainedDocuments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ermNumberOfTrainedDocuments :: Lens.Lens' EntityRecognizerMetadata (Core.Maybe Core.Int)
ermNumberOfTrainedDocuments = Lens.field @"numberOfTrainedDocuments"
{-# INLINEABLE ermNumberOfTrainedDocuments #-}
{-# DEPRECATED numberOfTrainedDocuments "Use generic-lens or generic-optics with 'numberOfTrainedDocuments' instead"  #-}

instance Core.FromJSON EntityRecognizerMetadata where
        parseJSON
          = Core.withObject "EntityRecognizerMetadata" Core.$
              \ x ->
                EntityRecognizerMetadata' Core.<$>
                  (x Core..:? "EntityTypes") Core.<*> x Core..:? "EvaluationMetrics"
                    Core.<*> x Core..:? "NumberOfTestDocuments"
                    Core.<*> x Core..:? "NumberOfTrainedDocuments"
