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
-- Module      : Network.AWS.Comprehend.Types.EntityRecognizerMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntityRecognizerMetadata where

import Network.AWS.Comprehend.Types.EntityRecognizerEvaluationMetrics
import Network.AWS.Comprehend.Types.EntityRecognizerMetadataEntityTypesListItem
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Detailed information about an entity recognizer.
--
-- /See:/ 'newEntityRecognizerMetadata' smart constructor.
data EntityRecognizerMetadata = EntityRecognizerMetadata'
  { -- | The number of documents in the input data that were used to test the
    -- entity recognizer. Typically this is 10 to 20 percent of the input
    -- documents.
    numberOfTestDocuments :: Prelude.Maybe Prelude.Int,
    -- | The number of documents in the input data that were used to train the
    -- entity recognizer. Typically this is 80 to 90 percent of the input
    -- documents.
    numberOfTrainedDocuments :: Prelude.Maybe Prelude.Int,
    -- | Detailed information about the accuracy of an entity recognizer.
    evaluationMetrics :: Prelude.Maybe EntityRecognizerEvaluationMetrics,
    -- | Entity types from the metadata of an entity recognizer.
    entityTypes :: Prelude.Maybe [EntityRecognizerMetadataEntityTypesListItem]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EntityRecognizerMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'numberOfTestDocuments', 'entityRecognizerMetadata_numberOfTestDocuments' - The number of documents in the input data that were used to test the
-- entity recognizer. Typically this is 10 to 20 percent of the input
-- documents.
--
-- 'numberOfTrainedDocuments', 'entityRecognizerMetadata_numberOfTrainedDocuments' - The number of documents in the input data that were used to train the
-- entity recognizer. Typically this is 80 to 90 percent of the input
-- documents.
--
-- 'evaluationMetrics', 'entityRecognizerMetadata_evaluationMetrics' - Detailed information about the accuracy of an entity recognizer.
--
-- 'entityTypes', 'entityRecognizerMetadata_entityTypes' - Entity types from the metadata of an entity recognizer.
newEntityRecognizerMetadata ::
  EntityRecognizerMetadata
newEntityRecognizerMetadata =
  EntityRecognizerMetadata'
    { numberOfTestDocuments =
        Prelude.Nothing,
      numberOfTrainedDocuments = Prelude.Nothing,
      evaluationMetrics = Prelude.Nothing,
      entityTypes = Prelude.Nothing
    }

-- | The number of documents in the input data that were used to test the
-- entity recognizer. Typically this is 10 to 20 percent of the input
-- documents.
entityRecognizerMetadata_numberOfTestDocuments :: Lens.Lens' EntityRecognizerMetadata (Prelude.Maybe Prelude.Int)
entityRecognizerMetadata_numberOfTestDocuments = Lens.lens (\EntityRecognizerMetadata' {numberOfTestDocuments} -> numberOfTestDocuments) (\s@EntityRecognizerMetadata' {} a -> s {numberOfTestDocuments = a} :: EntityRecognizerMetadata)

-- | The number of documents in the input data that were used to train the
-- entity recognizer. Typically this is 80 to 90 percent of the input
-- documents.
entityRecognizerMetadata_numberOfTrainedDocuments :: Lens.Lens' EntityRecognizerMetadata (Prelude.Maybe Prelude.Int)
entityRecognizerMetadata_numberOfTrainedDocuments = Lens.lens (\EntityRecognizerMetadata' {numberOfTrainedDocuments} -> numberOfTrainedDocuments) (\s@EntityRecognizerMetadata' {} a -> s {numberOfTrainedDocuments = a} :: EntityRecognizerMetadata)

-- | Detailed information about the accuracy of an entity recognizer.
entityRecognizerMetadata_evaluationMetrics :: Lens.Lens' EntityRecognizerMetadata (Prelude.Maybe EntityRecognizerEvaluationMetrics)
entityRecognizerMetadata_evaluationMetrics = Lens.lens (\EntityRecognizerMetadata' {evaluationMetrics} -> evaluationMetrics) (\s@EntityRecognizerMetadata' {} a -> s {evaluationMetrics = a} :: EntityRecognizerMetadata)

-- | Entity types from the metadata of an entity recognizer.
entityRecognizerMetadata_entityTypes :: Lens.Lens' EntityRecognizerMetadata (Prelude.Maybe [EntityRecognizerMetadataEntityTypesListItem])
entityRecognizerMetadata_entityTypes = Lens.lens (\EntityRecognizerMetadata' {entityTypes} -> entityTypes) (\s@EntityRecognizerMetadata' {} a -> s {entityTypes = a} :: EntityRecognizerMetadata) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON EntityRecognizerMetadata where
  parseJSON =
    Prelude.withObject
      "EntityRecognizerMetadata"
      ( \x ->
          EntityRecognizerMetadata'
            Prelude.<$> (x Prelude..:? "NumberOfTestDocuments")
            Prelude.<*> (x Prelude..:? "NumberOfTrainedDocuments")
            Prelude.<*> (x Prelude..:? "EvaluationMetrics")
            Prelude.<*> ( x Prelude..:? "EntityTypes"
                            Prelude..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable EntityRecognizerMetadata

instance Prelude.NFData EntityRecognizerMetadata
