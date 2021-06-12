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
-- Module      : Network.AWS.Comprehend.Types.EntityRecognizerInputDataConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntityRecognizerInputDataConfig where

import Network.AWS.Comprehend.Types.AugmentedManifestsListItem
import Network.AWS.Comprehend.Types.EntityRecognizerAnnotations
import Network.AWS.Comprehend.Types.EntityRecognizerDataFormat
import Network.AWS.Comprehend.Types.EntityRecognizerDocuments
import Network.AWS.Comprehend.Types.EntityRecognizerEntityList
import Network.AWS.Comprehend.Types.EntityTypesListItem
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies the format and location of the input data.
--
-- /See:/ 'newEntityRecognizerInputDataConfig' smart constructor.
data EntityRecognizerInputDataConfig = EntityRecognizerInputDataConfig'
  { -- | The S3 location of the folder that contains the training documents for
    -- your custom entity recognizer.
    --
    -- This parameter is required if you set @DataFormat@ to @COMPREHEND_CSV@.
    documents :: Core.Maybe EntityRecognizerDocuments,
    -- | A list of augmented manifest files that provide training data for your
    -- custom model. An augmented manifest file is a labeled dataset that is
    -- produced by Amazon SageMaker Ground Truth.
    --
    -- This parameter is required if you set @DataFormat@ to
    -- @AUGMENTED_MANIFEST@.
    augmentedManifests :: Core.Maybe [AugmentedManifestsListItem],
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
    dataFormat :: Core.Maybe EntityRecognizerDataFormat,
    -- | The S3 location of the CSV file that annotates your training documents.
    annotations :: Core.Maybe EntityRecognizerAnnotations,
    -- | The S3 location of the CSV file that has the entity list for your custom
    -- entity recognizer.
    entityList :: Core.Maybe EntityRecognizerEntityList,
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
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EntityRecognizerInputDataConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'annotations', 'entityRecognizerInputDataConfig_annotations' - The S3 location of the CSV file that annotates your training documents.
--
-- 'entityList', 'entityRecognizerInputDataConfig_entityList' - The S3 location of the CSV file that has the entity list for your custom
-- entity recognizer.
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
    { documents =
        Core.Nothing,
      augmentedManifests = Core.Nothing,
      dataFormat = Core.Nothing,
      annotations = Core.Nothing,
      entityList = Core.Nothing,
      entityTypes = Core.mempty
    }

-- | The S3 location of the folder that contains the training documents for
-- your custom entity recognizer.
--
-- This parameter is required if you set @DataFormat@ to @COMPREHEND_CSV@.
entityRecognizerInputDataConfig_documents :: Lens.Lens' EntityRecognizerInputDataConfig (Core.Maybe EntityRecognizerDocuments)
entityRecognizerInputDataConfig_documents = Lens.lens (\EntityRecognizerInputDataConfig' {documents} -> documents) (\s@EntityRecognizerInputDataConfig' {} a -> s {documents = a} :: EntityRecognizerInputDataConfig)

-- | A list of augmented manifest files that provide training data for your
-- custom model. An augmented manifest file is a labeled dataset that is
-- produced by Amazon SageMaker Ground Truth.
--
-- This parameter is required if you set @DataFormat@ to
-- @AUGMENTED_MANIFEST@.
entityRecognizerInputDataConfig_augmentedManifests :: Lens.Lens' EntityRecognizerInputDataConfig (Core.Maybe [AugmentedManifestsListItem])
entityRecognizerInputDataConfig_augmentedManifests = Lens.lens (\EntityRecognizerInputDataConfig' {augmentedManifests} -> augmentedManifests) (\s@EntityRecognizerInputDataConfig' {} a -> s {augmentedManifests = a} :: EntityRecognizerInputDataConfig) Core.. Lens.mapping Lens._Coerce

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
entityRecognizerInputDataConfig_dataFormat :: Lens.Lens' EntityRecognizerInputDataConfig (Core.Maybe EntityRecognizerDataFormat)
entityRecognizerInputDataConfig_dataFormat = Lens.lens (\EntityRecognizerInputDataConfig' {dataFormat} -> dataFormat) (\s@EntityRecognizerInputDataConfig' {} a -> s {dataFormat = a} :: EntityRecognizerInputDataConfig)

-- | The S3 location of the CSV file that annotates your training documents.
entityRecognizerInputDataConfig_annotations :: Lens.Lens' EntityRecognizerInputDataConfig (Core.Maybe EntityRecognizerAnnotations)
entityRecognizerInputDataConfig_annotations = Lens.lens (\EntityRecognizerInputDataConfig' {annotations} -> annotations) (\s@EntityRecognizerInputDataConfig' {} a -> s {annotations = a} :: EntityRecognizerInputDataConfig)

-- | The S3 location of the CSV file that has the entity list for your custom
-- entity recognizer.
entityRecognizerInputDataConfig_entityList :: Lens.Lens' EntityRecognizerInputDataConfig (Core.Maybe EntityRecognizerEntityList)
entityRecognizerInputDataConfig_entityList = Lens.lens (\EntityRecognizerInputDataConfig' {entityList} -> entityList) (\s@EntityRecognizerInputDataConfig' {} a -> s {entityList = a} :: EntityRecognizerInputDataConfig)

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
entityRecognizerInputDataConfig_entityTypes = Lens.lens (\EntityRecognizerInputDataConfig' {entityTypes} -> entityTypes) (\s@EntityRecognizerInputDataConfig' {} a -> s {entityTypes = a} :: EntityRecognizerInputDataConfig) Core.. Lens._Coerce

instance
  Core.FromJSON
    EntityRecognizerInputDataConfig
  where
  parseJSON =
    Core.withObject
      "EntityRecognizerInputDataConfig"
      ( \x ->
          EntityRecognizerInputDataConfig'
            Core.<$> (x Core..:? "Documents")
            Core.<*> ( x Core..:? "AugmentedManifests"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "DataFormat")
            Core.<*> (x Core..:? "Annotations")
            Core.<*> (x Core..:? "EntityList")
            Core.<*> (x Core..:? "EntityTypes" Core..!= Core.mempty)
      )

instance
  Core.Hashable
    EntityRecognizerInputDataConfig

instance Core.NFData EntityRecognizerInputDataConfig

instance Core.ToJSON EntityRecognizerInputDataConfig where
  toJSON EntityRecognizerInputDataConfig' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Documents" Core..=) Core.<$> documents,
            ("AugmentedManifests" Core..=)
              Core.<$> augmentedManifests,
            ("DataFormat" Core..=) Core.<$> dataFormat,
            ("Annotations" Core..=) Core.<$> annotations,
            ("EntityList" Core..=) Core.<$> entityList,
            Core.Just ("EntityTypes" Core..= entityTypes)
          ]
      )
