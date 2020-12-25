{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EntityRecognizerMetadataEntityTypesListItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntityRecognizerMetadataEntityTypesListItem
  ( EntityRecognizerMetadataEntityTypesListItem (..),

    -- * Smart constructor
    mkEntityRecognizerMetadataEntityTypesListItem,

    -- * Lenses
    ermetliEvaluationMetrics,
    ermetliNumberOfTrainMentions,
    ermetliType,
  )
where

import qualified Network.AWS.Comprehend.Types.AnyLengthString as Types
import qualified Network.AWS.Comprehend.Types.EntityTypesEvaluationMetrics as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Individual item from the list of entity types in the metadata of an entity recognizer.
--
-- /See:/ 'mkEntityRecognizerMetadataEntityTypesListItem' smart constructor.
data EntityRecognizerMetadataEntityTypesListItem = EntityRecognizerMetadataEntityTypesListItem'
  { -- | Detailed information about the accuracy of the entity recognizer for a specific item on the list of entity types.
    evaluationMetrics :: Core.Maybe Types.EntityTypesEvaluationMetrics,
    -- | Indicates the number of times the given entity type was seen in the training data.
    numberOfTrainMentions :: Core.Maybe Core.Int,
    -- | Type of entity from the list of entity types in the metadata of an entity recognizer.
    type' :: Core.Maybe Types.AnyLengthString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EntityRecognizerMetadataEntityTypesListItem' value with any optional fields omitted.
mkEntityRecognizerMetadataEntityTypesListItem ::
  EntityRecognizerMetadataEntityTypesListItem
mkEntityRecognizerMetadataEntityTypesListItem =
  EntityRecognizerMetadataEntityTypesListItem'
    { evaluationMetrics =
        Core.Nothing,
      numberOfTrainMentions = Core.Nothing,
      type' = Core.Nothing
    }

-- | Detailed information about the accuracy of the entity recognizer for a specific item on the list of entity types.
--
-- /Note:/ Consider using 'evaluationMetrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ermetliEvaluationMetrics :: Lens.Lens' EntityRecognizerMetadataEntityTypesListItem (Core.Maybe Types.EntityTypesEvaluationMetrics)
ermetliEvaluationMetrics = Lens.field @"evaluationMetrics"
{-# DEPRECATED ermetliEvaluationMetrics "Use generic-lens or generic-optics with 'evaluationMetrics' instead." #-}

-- | Indicates the number of times the given entity type was seen in the training data.
--
-- /Note:/ Consider using 'numberOfTrainMentions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ermetliNumberOfTrainMentions :: Lens.Lens' EntityRecognizerMetadataEntityTypesListItem (Core.Maybe Core.Int)
ermetliNumberOfTrainMentions = Lens.field @"numberOfTrainMentions"
{-# DEPRECATED ermetliNumberOfTrainMentions "Use generic-lens or generic-optics with 'numberOfTrainMentions' instead." #-}

-- | Type of entity from the list of entity types in the metadata of an entity recognizer.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ermetliType :: Lens.Lens' EntityRecognizerMetadataEntityTypesListItem (Core.Maybe Types.AnyLengthString)
ermetliType = Lens.field @"type'"
{-# DEPRECATED ermetliType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON EntityRecognizerMetadataEntityTypesListItem where
  parseJSON =
    Core.withObject "EntityRecognizerMetadataEntityTypesListItem" Core.$
      \x ->
        EntityRecognizerMetadataEntityTypesListItem'
          Core.<$> (x Core..:? "EvaluationMetrics")
          Core.<*> (x Core..:? "NumberOfTrainMentions")
          Core.<*> (x Core..:? "Type")
