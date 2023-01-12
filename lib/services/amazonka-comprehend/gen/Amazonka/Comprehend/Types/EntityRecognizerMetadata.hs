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
-- Module      : Amazonka.Comprehend.Types.EntityRecognizerMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.EntityRecognizerMetadata where

import Amazonka.Comprehend.Types.EntityRecognizerEvaluationMetrics
import Amazonka.Comprehend.Types.EntityRecognizerMetadataEntityTypesListItem
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Detailed information about an entity recognizer.
--
-- /See:/ 'newEntityRecognizerMetadata' smart constructor.
data EntityRecognizerMetadata = EntityRecognizerMetadata'
  { -- | Entity types from the metadata of an entity recognizer.
    entityTypes :: Prelude.Maybe [EntityRecognizerMetadataEntityTypesListItem],
    -- | Detailed information about the accuracy of an entity recognizer.
    evaluationMetrics :: Prelude.Maybe EntityRecognizerEvaluationMetrics,
    -- | The number of documents in the input data that were used to test the
    -- entity recognizer. Typically this is 10 to 20 percent of the input
    -- documents.
    numberOfTestDocuments :: Prelude.Maybe Prelude.Int,
    -- | The number of documents in the input data that were used to train the
    -- entity recognizer. Typically this is 80 to 90 percent of the input
    -- documents.
    numberOfTrainedDocuments :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EntityRecognizerMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'entityTypes', 'entityRecognizerMetadata_entityTypes' - Entity types from the metadata of an entity recognizer.
--
-- 'evaluationMetrics', 'entityRecognizerMetadata_evaluationMetrics' - Detailed information about the accuracy of an entity recognizer.
--
-- 'numberOfTestDocuments', 'entityRecognizerMetadata_numberOfTestDocuments' - The number of documents in the input data that were used to test the
-- entity recognizer. Typically this is 10 to 20 percent of the input
-- documents.
--
-- 'numberOfTrainedDocuments', 'entityRecognizerMetadata_numberOfTrainedDocuments' - The number of documents in the input data that were used to train the
-- entity recognizer. Typically this is 80 to 90 percent of the input
-- documents.
newEntityRecognizerMetadata ::
  EntityRecognizerMetadata
newEntityRecognizerMetadata =
  EntityRecognizerMetadata'
    { entityTypes =
        Prelude.Nothing,
      evaluationMetrics = Prelude.Nothing,
      numberOfTestDocuments = Prelude.Nothing,
      numberOfTrainedDocuments = Prelude.Nothing
    }

-- | Entity types from the metadata of an entity recognizer.
entityRecognizerMetadata_entityTypes :: Lens.Lens' EntityRecognizerMetadata (Prelude.Maybe [EntityRecognizerMetadataEntityTypesListItem])
entityRecognizerMetadata_entityTypes = Lens.lens (\EntityRecognizerMetadata' {entityTypes} -> entityTypes) (\s@EntityRecognizerMetadata' {} a -> s {entityTypes = a} :: EntityRecognizerMetadata) Prelude.. Lens.mapping Lens.coerced

-- | Detailed information about the accuracy of an entity recognizer.
entityRecognizerMetadata_evaluationMetrics :: Lens.Lens' EntityRecognizerMetadata (Prelude.Maybe EntityRecognizerEvaluationMetrics)
entityRecognizerMetadata_evaluationMetrics = Lens.lens (\EntityRecognizerMetadata' {evaluationMetrics} -> evaluationMetrics) (\s@EntityRecognizerMetadata' {} a -> s {evaluationMetrics = a} :: EntityRecognizerMetadata)

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

instance Data.FromJSON EntityRecognizerMetadata where
  parseJSON =
    Data.withObject
      "EntityRecognizerMetadata"
      ( \x ->
          EntityRecognizerMetadata'
            Prelude.<$> (x Data..:? "EntityTypes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "EvaluationMetrics")
            Prelude.<*> (x Data..:? "NumberOfTestDocuments")
            Prelude.<*> (x Data..:? "NumberOfTrainedDocuments")
      )

instance Prelude.Hashable EntityRecognizerMetadata where
  hashWithSalt _salt EntityRecognizerMetadata' {..} =
    _salt `Prelude.hashWithSalt` entityTypes
      `Prelude.hashWithSalt` evaluationMetrics
      `Prelude.hashWithSalt` numberOfTestDocuments
      `Prelude.hashWithSalt` numberOfTrainedDocuments

instance Prelude.NFData EntityRecognizerMetadata where
  rnf EntityRecognizerMetadata' {..} =
    Prelude.rnf entityTypes
      `Prelude.seq` Prelude.rnf evaluationMetrics
      `Prelude.seq` Prelude.rnf numberOfTestDocuments
      `Prelude.seq` Prelude.rnf numberOfTrainedDocuments
