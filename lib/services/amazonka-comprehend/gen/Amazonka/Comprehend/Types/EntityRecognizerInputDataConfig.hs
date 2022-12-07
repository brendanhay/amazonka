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
-- Module      : Amazonka.Comprehend.Types.EntityRecognizerInputDataConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.EntityRecognizerInputDataConfig where

import Amazonka.Comprehend.Types.AugmentedManifestsListItem
import Amazonka.Comprehend.Types.EntityRecognizerAnnotations
import Amazonka.Comprehend.Types.EntityRecognizerDataFormat
import Amazonka.Comprehend.Types.EntityRecognizerDocuments
import Amazonka.Comprehend.Types.EntityRecognizerEntityList
import Amazonka.Comprehend.Types.EntityTypesListItem
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the format and location of the input data.
--
-- /See:/ 'newEntityRecognizerInputDataConfig' smart constructor.
data EntityRecognizerInputDataConfig = EntityRecognizerInputDataConfig'
  { -- | The format of your training data:
    --
    -- -   @COMPREHEND_CSV@: A CSV file that supplements your training
    --     documents. The CSV file contains information about the custom
    --     entities that your trained model will detect. The required format of
    --     the file depends on whether you are providing annotations or an
    --     entity list.
    --
    --     If you use this value, you must provide your CSV file by using
    --     either the @Annotations@ or @EntityList@ parameters. You must
    --     provide your training documents by using the @Documents@ parameter.
    --
    -- -   @AUGMENTED_MANIFEST@: A labeled dataset that is produced by Amazon
    --     SageMaker Ground Truth. This file is in JSON lines format. Each line
    --     is a complete JSON object that contains a training document and its
    --     labels. Each label annotates a named entity in the training
    --     document.
    --
    --     If you use this value, you must provide the @AugmentedManifests@
    --     parameter in your request.
    --
    -- If you don\'t specify a value, Amazon Comprehend uses @COMPREHEND_CSV@
    -- as the default.
    dataFormat :: Prelude.Maybe EntityRecognizerDataFormat,
    -- | The S3 location of the CSV file that has the entity list for your custom
    -- entity recognizer.
    entityList :: Prelude.Maybe EntityRecognizerEntityList,
    -- | The S3 location of the CSV file that annotates your training documents.
    annotations :: Prelude.Maybe EntityRecognizerAnnotations,
    -- | The S3 location of the folder that contains the training documents for
    -- your custom entity recognizer.
    --
    -- This parameter is required if you set @DataFormat@ to @COMPREHEND_CSV@.
    documents :: Prelude.Maybe EntityRecognizerDocuments,
    -- | A list of augmented manifest files that provide training data for your
    -- custom model. An augmented manifest file is a labeled dataset that is
    -- produced by Amazon SageMaker Ground Truth.
    --
    -- This parameter is required if you set @DataFormat@ to
    -- @AUGMENTED_MANIFEST@.
    augmentedManifests :: Prelude.Maybe [AugmentedManifestsListItem],
    -- | The entity types in the labeled training data that Amazon Comprehend
    -- uses to train the custom entity recognizer. Any entity types that you
    -- don\'t specify are ignored.
    --
    -- A maximum of 25 entity types can be used at one time to train an entity
    -- recognizer. Entity types must not contain the following invalid
    -- characters: \\n (line break), \\\\n (escaped line break), \\r (carriage
    -- return), \\\\r (escaped carriage return), \\t (tab), \\\\t (escaped
    -- tab), space, and , (comma).
    entityTypes :: [EntityTypesListItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EntityRecognizerInputDataConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataFormat', 'entityRecognizerInputDataConfig_dataFormat' - The format of your training data:
--
-- -   @COMPREHEND_CSV@: A CSV file that supplements your training
--     documents. The CSV file contains information about the custom
--     entities that your trained model will detect. The required format of
--     the file depends on whether you are providing annotations or an
--     entity list.
--
--     If you use this value, you must provide your CSV file by using
--     either the @Annotations@ or @EntityList@ parameters. You must
--     provide your training documents by using the @Documents@ parameter.
--
-- -   @AUGMENTED_MANIFEST@: A labeled dataset that is produced by Amazon
--     SageMaker Ground Truth. This file is in JSON lines format. Each line
--     is a complete JSON object that contains a training document and its
--     labels. Each label annotates a named entity in the training
--     document.
--
--     If you use this value, you must provide the @AugmentedManifests@
--     parameter in your request.
--
-- If you don\'t specify a value, Amazon Comprehend uses @COMPREHEND_CSV@
-- as the default.
--
-- 'entityList', 'entityRecognizerInputDataConfig_entityList' - The S3 location of the CSV file that has the entity list for your custom
-- entity recognizer.
--
-- 'annotations', 'entityRecognizerInputDataConfig_annotations' - The S3 location of the CSV file that annotates your training documents.
--
-- 'documents', 'entityRecognizerInputDataConfig_documents' - The S3 location of the folder that contains the training documents for
-- your custom entity recognizer.
--
-- This parameter is required if you set @DataFormat@ to @COMPREHEND_CSV@.
--
-- 'augmentedManifests', 'entityRecognizerInputDataConfig_augmentedManifests' - A list of augmented manifest files that provide training data for your
-- custom model. An augmented manifest file is a labeled dataset that is
-- produced by Amazon SageMaker Ground Truth.
--
-- This parameter is required if you set @DataFormat@ to
-- @AUGMENTED_MANIFEST@.
--
-- 'entityTypes', 'entityRecognizerInputDataConfig_entityTypes' - The entity types in the labeled training data that Amazon Comprehend
-- uses to train the custom entity recognizer. Any entity types that you
-- don\'t specify are ignored.
--
-- A maximum of 25 entity types can be used at one time to train an entity
-- recognizer. Entity types must not contain the following invalid
-- characters: \\n (line break), \\\\n (escaped line break), \\r (carriage
-- return), \\\\r (escaped carriage return), \\t (tab), \\\\t (escaped
-- tab), space, and , (comma).
newEntityRecognizerInputDataConfig ::
  EntityRecognizerInputDataConfig
newEntityRecognizerInputDataConfig =
  EntityRecognizerInputDataConfig'
    { dataFormat =
        Prelude.Nothing,
      entityList = Prelude.Nothing,
      annotations = Prelude.Nothing,
      documents = Prelude.Nothing,
      augmentedManifests = Prelude.Nothing,
      entityTypes = Prelude.mempty
    }

-- | The format of your training data:
--
-- -   @COMPREHEND_CSV@: A CSV file that supplements your training
--     documents. The CSV file contains information about the custom
--     entities that your trained model will detect. The required format of
--     the file depends on whether you are providing annotations or an
--     entity list.
--
--     If you use this value, you must provide your CSV file by using
--     either the @Annotations@ or @EntityList@ parameters. You must
--     provide your training documents by using the @Documents@ parameter.
--
-- -   @AUGMENTED_MANIFEST@: A labeled dataset that is produced by Amazon
--     SageMaker Ground Truth. This file is in JSON lines format. Each line
--     is a complete JSON object that contains a training document and its
--     labels. Each label annotates a named entity in the training
--     document.
--
--     If you use this value, you must provide the @AugmentedManifests@
--     parameter in your request.
--
-- If you don\'t specify a value, Amazon Comprehend uses @COMPREHEND_CSV@
-- as the default.
entityRecognizerInputDataConfig_dataFormat :: Lens.Lens' EntityRecognizerInputDataConfig (Prelude.Maybe EntityRecognizerDataFormat)
entityRecognizerInputDataConfig_dataFormat = Lens.lens (\EntityRecognizerInputDataConfig' {dataFormat} -> dataFormat) (\s@EntityRecognizerInputDataConfig' {} a -> s {dataFormat = a} :: EntityRecognizerInputDataConfig)

-- | The S3 location of the CSV file that has the entity list for your custom
-- entity recognizer.
entityRecognizerInputDataConfig_entityList :: Lens.Lens' EntityRecognizerInputDataConfig (Prelude.Maybe EntityRecognizerEntityList)
entityRecognizerInputDataConfig_entityList = Lens.lens (\EntityRecognizerInputDataConfig' {entityList} -> entityList) (\s@EntityRecognizerInputDataConfig' {} a -> s {entityList = a} :: EntityRecognizerInputDataConfig)

-- | The S3 location of the CSV file that annotates your training documents.
entityRecognizerInputDataConfig_annotations :: Lens.Lens' EntityRecognizerInputDataConfig (Prelude.Maybe EntityRecognizerAnnotations)
entityRecognizerInputDataConfig_annotations = Lens.lens (\EntityRecognizerInputDataConfig' {annotations} -> annotations) (\s@EntityRecognizerInputDataConfig' {} a -> s {annotations = a} :: EntityRecognizerInputDataConfig)

-- | The S3 location of the folder that contains the training documents for
-- your custom entity recognizer.
--
-- This parameter is required if you set @DataFormat@ to @COMPREHEND_CSV@.
entityRecognizerInputDataConfig_documents :: Lens.Lens' EntityRecognizerInputDataConfig (Prelude.Maybe EntityRecognizerDocuments)
entityRecognizerInputDataConfig_documents = Lens.lens (\EntityRecognizerInputDataConfig' {documents} -> documents) (\s@EntityRecognizerInputDataConfig' {} a -> s {documents = a} :: EntityRecognizerInputDataConfig)

-- | A list of augmented manifest files that provide training data for your
-- custom model. An augmented manifest file is a labeled dataset that is
-- produced by Amazon SageMaker Ground Truth.
--
-- This parameter is required if you set @DataFormat@ to
-- @AUGMENTED_MANIFEST@.
entityRecognizerInputDataConfig_augmentedManifests :: Lens.Lens' EntityRecognizerInputDataConfig (Prelude.Maybe [AugmentedManifestsListItem])
entityRecognizerInputDataConfig_augmentedManifests = Lens.lens (\EntityRecognizerInputDataConfig' {augmentedManifests} -> augmentedManifests) (\s@EntityRecognizerInputDataConfig' {} a -> s {augmentedManifests = a} :: EntityRecognizerInputDataConfig) Prelude.. Lens.mapping Lens.coerced

-- | The entity types in the labeled training data that Amazon Comprehend
-- uses to train the custom entity recognizer. Any entity types that you
-- don\'t specify are ignored.
--
-- A maximum of 25 entity types can be used at one time to train an entity
-- recognizer. Entity types must not contain the following invalid
-- characters: \\n (line break), \\\\n (escaped line break), \\r (carriage
-- return), \\\\r (escaped carriage return), \\t (tab), \\\\t (escaped
-- tab), space, and , (comma).
entityRecognizerInputDataConfig_entityTypes :: Lens.Lens' EntityRecognizerInputDataConfig [EntityTypesListItem]
entityRecognizerInputDataConfig_entityTypes = Lens.lens (\EntityRecognizerInputDataConfig' {entityTypes} -> entityTypes) (\s@EntityRecognizerInputDataConfig' {} a -> s {entityTypes = a} :: EntityRecognizerInputDataConfig) Prelude.. Lens.coerced

instance
  Data.FromJSON
    EntityRecognizerInputDataConfig
  where
  parseJSON =
    Data.withObject
      "EntityRecognizerInputDataConfig"
      ( \x ->
          EntityRecognizerInputDataConfig'
            Prelude.<$> (x Data..:? "DataFormat")
            Prelude.<*> (x Data..:? "EntityList")
            Prelude.<*> (x Data..:? "Annotations")
            Prelude.<*> (x Data..:? "Documents")
            Prelude.<*> ( x Data..:? "AugmentedManifests"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "EntityTypes" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    EntityRecognizerInputDataConfig
  where
  hashWithSalt
    _salt
    EntityRecognizerInputDataConfig' {..} =
      _salt `Prelude.hashWithSalt` dataFormat
        `Prelude.hashWithSalt` entityList
        `Prelude.hashWithSalt` annotations
        `Prelude.hashWithSalt` documents
        `Prelude.hashWithSalt` augmentedManifests
        `Prelude.hashWithSalt` entityTypes

instance
  Prelude.NFData
    EntityRecognizerInputDataConfig
  where
  rnf EntityRecognizerInputDataConfig' {..} =
    Prelude.rnf dataFormat
      `Prelude.seq` Prelude.rnf entityList
      `Prelude.seq` Prelude.rnf annotations
      `Prelude.seq` Prelude.rnf documents
      `Prelude.seq` Prelude.rnf augmentedManifests
      `Prelude.seq` Prelude.rnf entityTypes

instance Data.ToJSON EntityRecognizerInputDataConfig where
  toJSON EntityRecognizerInputDataConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataFormat" Data..=) Prelude.<$> dataFormat,
            ("EntityList" Data..=) Prelude.<$> entityList,
            ("Annotations" Data..=) Prelude.<$> annotations,
            ("Documents" Data..=) Prelude.<$> documents,
            ("AugmentedManifests" Data..=)
              Prelude.<$> augmentedManifests,
            Prelude.Just ("EntityTypes" Data..= entityTypes)
          ]
      )
