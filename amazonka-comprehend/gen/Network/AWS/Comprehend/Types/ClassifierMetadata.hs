{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.ClassifierMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.ClassifierMetadata where

import Network.AWS.Comprehend.Types.ClassifierEvaluationMetrics
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about a document classifier.
--
-- /See:/ 'newClassifierMetadata' smart constructor.
data ClassifierMetadata = ClassifierMetadata'
  { -- | The number of labels in the input data.
    numberOfLabels :: Prelude.Maybe Prelude.Int,
    -- | The number of documents in the input data that were used to test the
    -- classifier. Typically this is 10 to 20 percent of the input documents,
    -- up to 10,000 documents.
    numberOfTestDocuments :: Prelude.Maybe Prelude.Int,
    -- | The number of documents in the input data that were used to train the
    -- classifier. Typically this is 80 to 90 percent of the input documents.
    numberOfTrainedDocuments :: Prelude.Maybe Prelude.Int,
    -- | Describes the result metrics for the test data associated with an
    -- documentation classifier.
    evaluationMetrics :: Prelude.Maybe ClassifierEvaluationMetrics
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ClassifierMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'numberOfLabels', 'classifierMetadata_numberOfLabels' - The number of labels in the input data.
--
-- 'numberOfTestDocuments', 'classifierMetadata_numberOfTestDocuments' - The number of documents in the input data that were used to test the
-- classifier. Typically this is 10 to 20 percent of the input documents,
-- up to 10,000 documents.
--
-- 'numberOfTrainedDocuments', 'classifierMetadata_numberOfTrainedDocuments' - The number of documents in the input data that were used to train the
-- classifier. Typically this is 80 to 90 percent of the input documents.
--
-- 'evaluationMetrics', 'classifierMetadata_evaluationMetrics' - Describes the result metrics for the test data associated with an
-- documentation classifier.
newClassifierMetadata ::
  ClassifierMetadata
newClassifierMetadata =
  ClassifierMetadata'
    { numberOfLabels =
        Prelude.Nothing,
      numberOfTestDocuments = Prelude.Nothing,
      numberOfTrainedDocuments = Prelude.Nothing,
      evaluationMetrics = Prelude.Nothing
    }

-- | The number of labels in the input data.
classifierMetadata_numberOfLabels :: Lens.Lens' ClassifierMetadata (Prelude.Maybe Prelude.Int)
classifierMetadata_numberOfLabels = Lens.lens (\ClassifierMetadata' {numberOfLabels} -> numberOfLabels) (\s@ClassifierMetadata' {} a -> s {numberOfLabels = a} :: ClassifierMetadata)

-- | The number of documents in the input data that were used to test the
-- classifier. Typically this is 10 to 20 percent of the input documents,
-- up to 10,000 documents.
classifierMetadata_numberOfTestDocuments :: Lens.Lens' ClassifierMetadata (Prelude.Maybe Prelude.Int)
classifierMetadata_numberOfTestDocuments = Lens.lens (\ClassifierMetadata' {numberOfTestDocuments} -> numberOfTestDocuments) (\s@ClassifierMetadata' {} a -> s {numberOfTestDocuments = a} :: ClassifierMetadata)

-- | The number of documents in the input data that were used to train the
-- classifier. Typically this is 80 to 90 percent of the input documents.
classifierMetadata_numberOfTrainedDocuments :: Lens.Lens' ClassifierMetadata (Prelude.Maybe Prelude.Int)
classifierMetadata_numberOfTrainedDocuments = Lens.lens (\ClassifierMetadata' {numberOfTrainedDocuments} -> numberOfTrainedDocuments) (\s@ClassifierMetadata' {} a -> s {numberOfTrainedDocuments = a} :: ClassifierMetadata)

-- | Describes the result metrics for the test data associated with an
-- documentation classifier.
classifierMetadata_evaluationMetrics :: Lens.Lens' ClassifierMetadata (Prelude.Maybe ClassifierEvaluationMetrics)
classifierMetadata_evaluationMetrics = Lens.lens (\ClassifierMetadata' {evaluationMetrics} -> evaluationMetrics) (\s@ClassifierMetadata' {} a -> s {evaluationMetrics = a} :: ClassifierMetadata)

instance Prelude.FromJSON ClassifierMetadata where
  parseJSON =
    Prelude.withObject
      "ClassifierMetadata"
      ( \x ->
          ClassifierMetadata'
            Prelude.<$> (x Prelude..:? "NumberOfLabels")
            Prelude.<*> (x Prelude..:? "NumberOfTestDocuments")
            Prelude.<*> (x Prelude..:? "NumberOfTrainedDocuments")
            Prelude.<*> (x Prelude..:? "EvaluationMetrics")
      )

instance Prelude.Hashable ClassifierMetadata

instance Prelude.NFData ClassifierMetadata
