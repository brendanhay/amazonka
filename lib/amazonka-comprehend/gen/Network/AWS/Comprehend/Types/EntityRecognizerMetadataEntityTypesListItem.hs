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
    ermetliType,
    ermetliNumberOfTrainMentions,
  )
where

import Network.AWS.Comprehend.Types.EntityTypesEvaluationMetrics
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Individual item from the list of entity types in the metadata of an entity recognizer.
--
-- /See:/ 'mkEntityRecognizerMetadataEntityTypesListItem' smart constructor.
data EntityRecognizerMetadataEntityTypesListItem = EntityRecognizerMetadataEntityTypesListItem'
  { evaluationMetrics ::
      Lude.Maybe
        EntityTypesEvaluationMetrics,
    type' ::
      Lude.Maybe
        Lude.Text,
    numberOfTrainMentions ::
      Lude.Maybe
        Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EntityRecognizerMetadataEntityTypesListItem' with the minimum fields required to make a request.
--
-- * 'evaluationMetrics' - Detailed information about the accuracy of the entity recognizer for a specific item on the list of entity types.
-- * 'numberOfTrainMentions' - Indicates the number of times the given entity type was seen in the training data.
-- * 'type'' - Type of entity from the list of entity types in the metadata of an entity recognizer.
mkEntityRecognizerMetadataEntityTypesListItem ::
  EntityRecognizerMetadataEntityTypesListItem
mkEntityRecognizerMetadataEntityTypesListItem =
  EntityRecognizerMetadataEntityTypesListItem'
    { evaluationMetrics =
        Lude.Nothing,
      type' = Lude.Nothing,
      numberOfTrainMentions = Lude.Nothing
    }

-- | Detailed information about the accuracy of the entity recognizer for a specific item on the list of entity types.
--
-- /Note:/ Consider using 'evaluationMetrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ermetliEvaluationMetrics :: Lens.Lens' EntityRecognizerMetadataEntityTypesListItem (Lude.Maybe EntityTypesEvaluationMetrics)
ermetliEvaluationMetrics = Lens.lens (evaluationMetrics :: EntityRecognizerMetadataEntityTypesListItem -> Lude.Maybe EntityTypesEvaluationMetrics) (\s a -> s {evaluationMetrics = a} :: EntityRecognizerMetadataEntityTypesListItem)
{-# DEPRECATED ermetliEvaluationMetrics "Use generic-lens or generic-optics with 'evaluationMetrics' instead." #-}

-- | Type of entity from the list of entity types in the metadata of an entity recognizer.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ermetliType :: Lens.Lens' EntityRecognizerMetadataEntityTypesListItem (Lude.Maybe Lude.Text)
ermetliType = Lens.lens (type' :: EntityRecognizerMetadataEntityTypesListItem -> Lude.Maybe Lude.Text) (\s a -> s {type' = a} :: EntityRecognizerMetadataEntityTypesListItem)
{-# DEPRECATED ermetliType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | Indicates the number of times the given entity type was seen in the training data.
--
-- /Note:/ Consider using 'numberOfTrainMentions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ermetliNumberOfTrainMentions :: Lens.Lens' EntityRecognizerMetadataEntityTypesListItem (Lude.Maybe Lude.Int)
ermetliNumberOfTrainMentions = Lens.lens (numberOfTrainMentions :: EntityRecognizerMetadataEntityTypesListItem -> Lude.Maybe Lude.Int) (\s a -> s {numberOfTrainMentions = a} :: EntityRecognizerMetadataEntityTypesListItem)
{-# DEPRECATED ermetliNumberOfTrainMentions "Use generic-lens or generic-optics with 'numberOfTrainMentions' instead." #-}

instance Lude.FromJSON EntityRecognizerMetadataEntityTypesListItem where
  parseJSON =
    Lude.withObject
      "EntityRecognizerMetadataEntityTypesListItem"
      ( \x ->
          EntityRecognizerMetadataEntityTypesListItem'
            Lude.<$> (x Lude..:? "EvaluationMetrics")
            Lude.<*> (x Lude..:? "Type")
            Lude.<*> (x Lude..:? "NumberOfTrainMentions")
      )
