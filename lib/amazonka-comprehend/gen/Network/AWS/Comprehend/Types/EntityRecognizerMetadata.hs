{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EntityRecognizerMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntityRecognizerMetadata
  ( EntityRecognizerMetadata (..),

    -- * Smart constructor
    mkEntityRecognizerMetadata,

    -- * Lenses
    ermEntityTypes,
    ermEvaluationMetrics,
    ermNumberOfTrainedDocuments,
    ermNumberOfTestDocuments,
  )
where

import Network.AWS.Comprehend.Types.EntityRecognizerEvaluationMetrics
import Network.AWS.Comprehend.Types.EntityRecognizerMetadataEntityTypesListItem
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Detailed information about an entity recognizer.
--
-- /See:/ 'mkEntityRecognizerMetadata' smart constructor.
data EntityRecognizerMetadata = EntityRecognizerMetadata'
  { -- | Entity types from the metadata of an entity recognizer.
    entityTypes :: Lude.Maybe [EntityRecognizerMetadataEntityTypesListItem],
    -- | Detailed information about the accuracy of an entity recognizer.
    evaluationMetrics :: Lude.Maybe EntityRecognizerEvaluationMetrics,
    -- | The number of documents in the input data that were used to train the entity recognizer. Typically this is 80 to 90 percent of the input documents.
    numberOfTrainedDocuments :: Lude.Maybe Lude.Int,
    -- | The number of documents in the input data that were used to test the entity recognizer. Typically this is 10 to 20 percent of the input documents.
    numberOfTestDocuments :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EntityRecognizerMetadata' with the minimum fields required to make a request.
--
-- * 'entityTypes' - Entity types from the metadata of an entity recognizer.
-- * 'evaluationMetrics' - Detailed information about the accuracy of an entity recognizer.
-- * 'numberOfTrainedDocuments' - The number of documents in the input data that were used to train the entity recognizer. Typically this is 80 to 90 percent of the input documents.
-- * 'numberOfTestDocuments' - The number of documents in the input data that were used to test the entity recognizer. Typically this is 10 to 20 percent of the input documents.
mkEntityRecognizerMetadata ::
  EntityRecognizerMetadata
mkEntityRecognizerMetadata =
  EntityRecognizerMetadata'
    { entityTypes = Lude.Nothing,
      evaluationMetrics = Lude.Nothing,
      numberOfTrainedDocuments = Lude.Nothing,
      numberOfTestDocuments = Lude.Nothing
    }

-- | Entity types from the metadata of an entity recognizer.
--
-- /Note:/ Consider using 'entityTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ermEntityTypes :: Lens.Lens' EntityRecognizerMetadata (Lude.Maybe [EntityRecognizerMetadataEntityTypesListItem])
ermEntityTypes = Lens.lens (entityTypes :: EntityRecognizerMetadata -> Lude.Maybe [EntityRecognizerMetadataEntityTypesListItem]) (\s a -> s {entityTypes = a} :: EntityRecognizerMetadata)
{-# DEPRECATED ermEntityTypes "Use generic-lens or generic-optics with 'entityTypes' instead." #-}

-- | Detailed information about the accuracy of an entity recognizer.
--
-- /Note:/ Consider using 'evaluationMetrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ermEvaluationMetrics :: Lens.Lens' EntityRecognizerMetadata (Lude.Maybe EntityRecognizerEvaluationMetrics)
ermEvaluationMetrics = Lens.lens (evaluationMetrics :: EntityRecognizerMetadata -> Lude.Maybe EntityRecognizerEvaluationMetrics) (\s a -> s {evaluationMetrics = a} :: EntityRecognizerMetadata)
{-# DEPRECATED ermEvaluationMetrics "Use generic-lens or generic-optics with 'evaluationMetrics' instead." #-}

-- | The number of documents in the input data that were used to train the entity recognizer. Typically this is 80 to 90 percent of the input documents.
--
-- /Note:/ Consider using 'numberOfTrainedDocuments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ermNumberOfTrainedDocuments :: Lens.Lens' EntityRecognizerMetadata (Lude.Maybe Lude.Int)
ermNumberOfTrainedDocuments = Lens.lens (numberOfTrainedDocuments :: EntityRecognizerMetadata -> Lude.Maybe Lude.Int) (\s a -> s {numberOfTrainedDocuments = a} :: EntityRecognizerMetadata)
{-# DEPRECATED ermNumberOfTrainedDocuments "Use generic-lens or generic-optics with 'numberOfTrainedDocuments' instead." #-}

-- | The number of documents in the input data that were used to test the entity recognizer. Typically this is 10 to 20 percent of the input documents.
--
-- /Note:/ Consider using 'numberOfTestDocuments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ermNumberOfTestDocuments :: Lens.Lens' EntityRecognizerMetadata (Lude.Maybe Lude.Int)
ermNumberOfTestDocuments = Lens.lens (numberOfTestDocuments :: EntityRecognizerMetadata -> Lude.Maybe Lude.Int) (\s a -> s {numberOfTestDocuments = a} :: EntityRecognizerMetadata)
{-# DEPRECATED ermNumberOfTestDocuments "Use generic-lens or generic-optics with 'numberOfTestDocuments' instead." #-}

instance Lude.FromJSON EntityRecognizerMetadata where
  parseJSON =
    Lude.withObject
      "EntityRecognizerMetadata"
      ( \x ->
          EntityRecognizerMetadata'
            Lude.<$> (x Lude..:? "EntityTypes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "EvaluationMetrics")
            Lude.<*> (x Lude..:? "NumberOfTrainedDocuments")
            Lude.<*> (x Lude..:? "NumberOfTestDocuments")
      )
