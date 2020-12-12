{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.ClassifierMetadata
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.ClassifierMetadata
  ( ClassifierMetadata (..),

    -- * Smart constructor
    mkClassifierMetadata,

    -- * Lenses
    cmNumberOfLabels,
    cmEvaluationMetrics,
    cmNumberOfTrainedDocuments,
    cmNumberOfTestDocuments,
  )
where

import Network.AWS.Comprehend.Types.ClassifierEvaluationMetrics
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about a document classifier.
--
-- /See:/ 'mkClassifierMetadata' smart constructor.
data ClassifierMetadata = ClassifierMetadata'
  { numberOfLabels ::
      Lude.Maybe Lude.Int,
    evaluationMetrics ::
      Lude.Maybe ClassifierEvaluationMetrics,
    numberOfTrainedDocuments :: Lude.Maybe Lude.Int,
    numberOfTestDocuments :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClassifierMetadata' with the minimum fields required to make a request.
--
-- * 'evaluationMetrics' - Describes the result metrics for the test data associated with an documentation classifier.
-- * 'numberOfLabels' - The number of labels in the input data.
-- * 'numberOfTestDocuments' - The number of documents in the input data that were used to test the classifier. Typically this is 10 to 20 percent of the input documents, up to 10,000 documents.
-- * 'numberOfTrainedDocuments' - The number of documents in the input data that were used to train the classifier. Typically this is 80 to 90 percent of the input documents.
mkClassifierMetadata ::
  ClassifierMetadata
mkClassifierMetadata =
  ClassifierMetadata'
    { numberOfLabels = Lude.Nothing,
      evaluationMetrics = Lude.Nothing,
      numberOfTrainedDocuments = Lude.Nothing,
      numberOfTestDocuments = Lude.Nothing
    }

-- | The number of labels in the input data.
--
-- /Note:/ Consider using 'numberOfLabels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmNumberOfLabels :: Lens.Lens' ClassifierMetadata (Lude.Maybe Lude.Int)
cmNumberOfLabels = Lens.lens (numberOfLabels :: ClassifierMetadata -> Lude.Maybe Lude.Int) (\s a -> s {numberOfLabels = a} :: ClassifierMetadata)
{-# DEPRECATED cmNumberOfLabels "Use generic-lens or generic-optics with 'numberOfLabels' instead." #-}

-- | Describes the result metrics for the test data associated with an documentation classifier.
--
-- /Note:/ Consider using 'evaluationMetrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmEvaluationMetrics :: Lens.Lens' ClassifierMetadata (Lude.Maybe ClassifierEvaluationMetrics)
cmEvaluationMetrics = Lens.lens (evaluationMetrics :: ClassifierMetadata -> Lude.Maybe ClassifierEvaluationMetrics) (\s a -> s {evaluationMetrics = a} :: ClassifierMetadata)
{-# DEPRECATED cmEvaluationMetrics "Use generic-lens or generic-optics with 'evaluationMetrics' instead." #-}

-- | The number of documents in the input data that were used to train the classifier. Typically this is 80 to 90 percent of the input documents.
--
-- /Note:/ Consider using 'numberOfTrainedDocuments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmNumberOfTrainedDocuments :: Lens.Lens' ClassifierMetadata (Lude.Maybe Lude.Int)
cmNumberOfTrainedDocuments = Lens.lens (numberOfTrainedDocuments :: ClassifierMetadata -> Lude.Maybe Lude.Int) (\s a -> s {numberOfTrainedDocuments = a} :: ClassifierMetadata)
{-# DEPRECATED cmNumberOfTrainedDocuments "Use generic-lens or generic-optics with 'numberOfTrainedDocuments' instead." #-}

-- | The number of documents in the input data that were used to test the classifier. Typically this is 10 to 20 percent of the input documents, up to 10,000 documents.
--
-- /Note:/ Consider using 'numberOfTestDocuments' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmNumberOfTestDocuments :: Lens.Lens' ClassifierMetadata (Lude.Maybe Lude.Int)
cmNumberOfTestDocuments = Lens.lens (numberOfTestDocuments :: ClassifierMetadata -> Lude.Maybe Lude.Int) (\s a -> s {numberOfTestDocuments = a} :: ClassifierMetadata)
{-# DEPRECATED cmNumberOfTestDocuments "Use generic-lens or generic-optics with 'numberOfTestDocuments' instead." #-}

instance Lude.FromJSON ClassifierMetadata where
  parseJSON =
    Lude.withObject
      "ClassifierMetadata"
      ( \x ->
          ClassifierMetadata'
            Lude.<$> (x Lude..:? "NumberOfLabels")
            Lude.<*> (x Lude..:? "EvaluationMetrics")
            Lude.<*> (x Lude..:? "NumberOfTrainedDocuments")
            Lude.<*> (x Lude..:? "NumberOfTestDocuments")
      )
