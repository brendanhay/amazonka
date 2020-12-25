{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EntityRecognizerInputDataConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntityRecognizerInputDataConfig
  ( EntityRecognizerInputDataConfig (..),

    -- * Smart constructor
    mkEntityRecognizerInputDataConfig,

    -- * Lenses
    eridcEntityTypes,
    eridcAnnotations,
    eridcAugmentedManifests,
    eridcDataFormat,
    eridcDocuments,
    eridcEntityList,
  )
where

import qualified Network.AWS.Comprehend.Types.AugmentedManifestsListItem as Types
import qualified Network.AWS.Comprehend.Types.EntityRecognizerAnnotations as Types
import qualified Network.AWS.Comprehend.Types.EntityRecognizerDataFormat as Types
import qualified Network.AWS.Comprehend.Types.EntityRecognizerDocuments as Types
import qualified Network.AWS.Comprehend.Types.EntityRecognizerEntityList as Types
import qualified Network.AWS.Comprehend.Types.EntityTypesListItem as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the format and location of the input data.
--
-- /See:/ 'mkEntityRecognizerInputDataConfig' smart constructor.
data EntityRecognizerInputDataConfig = EntityRecognizerInputDataConfig'
  { -- | The entity types in the labeled training data that Amazon Comprehend uses to train the custom entity recognizer. Any entity types that you don't specify are ignored.
    --
    -- A maximum of 25 entity types can be used at one time to train an entity recognizer. Entity types must not contain the following invalid characters: \n (line break), \\n (escaped line break), \r (carriage return), \\r (escaped carriage return), \t (tab), \\t (escaped tab), space, and , (comma).
    entityTypes :: [Types.EntityTypesListItem],
    -- | The S3 location of the CSV file that annotates your training documents.
    annotations :: Core.Maybe Types.EntityRecognizerAnnotations,
    -- | A list of augmented manifest files that provide training data for your custom model. An augmented manifest file is a labeled dataset that is produced by Amazon SageMaker Ground Truth.
    --
    -- This parameter is required if you set @DataFormat@ to @AUGMENTED_MANIFEST@ .
    augmentedManifests :: Core.Maybe [Types.AugmentedManifestsListItem],
    -- | The format of your training data:
    --
    --
    --     * @COMPREHEND_CSV@ : A CSV file that supplements your training documents. The CSV file contains information about the custom entities that your trained model will detect. The required format of the file depends on whether you are providing annotations or an entity list.
    -- If you use this value, you must provide your CSV file by using either the @Annotations@ or @EntityList@ parameters. You must provide your training documents by using the @Documents@ parameter.
    --
    --
    --     * @AUGMENTED_MANIFEST@ : A labeled dataset that is produced by Amazon SageMaker Ground Truth. This file is in JSON lines format. Each line is a complete JSON object that contains a training document and its labels. Each label annotates a named entity in the training document.
    -- If you use this value, you must provide the @AugmentedManifests@ parameter in your request.
    --
    --
    -- If you don't specify a value, Amazon Comprehend uses @COMPREHEND_CSV@ as the default.
    dataFormat :: Core.Maybe Types.EntityRecognizerDataFormat,
    -- | The S3 location of the folder that contains the training documents for your custom entity recognizer.
    --
    -- This parameter is required if you set @DataFormat@ to @COMPREHEND_CSV@ .
    documents :: Core.Maybe Types.EntityRecognizerDocuments,
    -- | The S3 location of the CSV file that has the entity list for your custom entity recognizer.
    entityList :: Core.Maybe Types.EntityRecognizerEntityList
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EntityRecognizerInputDataConfig' value with any optional fields omitted.
mkEntityRecognizerInputDataConfig ::
  EntityRecognizerInputDataConfig
mkEntityRecognizerInputDataConfig =
  EntityRecognizerInputDataConfig'
    { entityTypes = Core.mempty,
      annotations = Core.Nothing,
      augmentedManifests = Core.Nothing,
      dataFormat = Core.Nothing,
      documents = Core.Nothing,
      entityList = Core.Nothing
    }

-- | The entity types in the labeled training data that Amazon Comprehend uses to train the custom entity recognizer. Any entity types that you don't specify are ignored.
--
-- A maximum of 25 entity types can be used at one time to train an entity recognizer. Entity types must not contain the following invalid characters: \n (line break), \\n (escaped line break), \r (carriage return), \\r (escaped carriage return), \t (tab), \\t (escaped tab), space, and , (comma).
--
-- /Note:/ Consider using 'entityTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eridcEntityTypes :: Lens.Lens' EntityRecognizerInputDataConfig [Types.EntityTypesListItem]
eridcEntityTypes = Lens.field @"entityTypes"
{-# DEPRECATED eridcEntityTypes "Use generic-lens or generic-optics with 'entityTypes' instead." #-}

-- | The S3 location of the CSV file that annotates your training documents.
--
-- /Note:/ Consider using 'annotations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eridcAnnotations :: Lens.Lens' EntityRecognizerInputDataConfig (Core.Maybe Types.EntityRecognizerAnnotations)
eridcAnnotations = Lens.field @"annotations"
{-# DEPRECATED eridcAnnotations "Use generic-lens or generic-optics with 'annotations' instead." #-}

-- | A list of augmented manifest files that provide training data for your custom model. An augmented manifest file is a labeled dataset that is produced by Amazon SageMaker Ground Truth.
--
-- This parameter is required if you set @DataFormat@ to @AUGMENTED_MANIFEST@ .
--
-- /Note:/ Consider using 'augmentedManifests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eridcAugmentedManifests :: Lens.Lens' EntityRecognizerInputDataConfig (Core.Maybe [Types.AugmentedManifestsListItem])
eridcAugmentedManifests = Lens.field @"augmentedManifests"
{-# DEPRECATED eridcAugmentedManifests "Use generic-lens or generic-optics with 'augmentedManifests' instead." #-}

-- | The format of your training data:
--
--
--     * @COMPREHEND_CSV@ : A CSV file that supplements your training documents. The CSV file contains information about the custom entities that your trained model will detect. The required format of the file depends on whether you are providing annotations or an entity list.
-- If you use this value, you must provide your CSV file by using either the @Annotations@ or @EntityList@ parameters. You must provide your training documents by using the @Documents@ parameter.
--
--
--     * @AUGMENTED_MANIFEST@ : A labeled dataset that is produced by Amazon SageMaker Ground Truth. This file is in JSON lines format. Each line is a complete JSON object that contains a training document and its labels. Each label annotates a named entity in the training document.
-- If you use this value, you must provide the @AugmentedManifests@ parameter in your request.
--
--
-- If you don't specify a value, Amazon Comprehend uses @COMPREHEND_CSV@ as the default.
--
-- /Note:/ Consider using 'dataFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eridcDataFormat :: Lens.Lens' EntityRecognizerInputDataConfig (Core.Maybe Types.EntityRecognizerDataFormat)
eridcDataFormat = Lens.field @"dataFormat"
{-# DEPRECATED eridcDataFormat "Use generic-lens or generic-optics with 'dataFormat' instead." #-}

-- | The S3 location of the folder that contains the training documents for your custom entity recognizer.
--
-- This parameter is required if you set @DataFormat@ to @COMPREHEND_CSV@ .
--
-- /Note:/ Consider using 'documents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eridcDocuments :: Lens.Lens' EntityRecognizerInputDataConfig (Core.Maybe Types.EntityRecognizerDocuments)
eridcDocuments = Lens.field @"documents"
{-# DEPRECATED eridcDocuments "Use generic-lens or generic-optics with 'documents' instead." #-}

-- | The S3 location of the CSV file that has the entity list for your custom entity recognizer.
--
-- /Note:/ Consider using 'entityList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eridcEntityList :: Lens.Lens' EntityRecognizerInputDataConfig (Core.Maybe Types.EntityRecognizerEntityList)
eridcEntityList = Lens.field @"entityList"
{-# DEPRECATED eridcEntityList "Use generic-lens or generic-optics with 'entityList' instead." #-}

instance Core.FromJSON EntityRecognizerInputDataConfig where
  toJSON EntityRecognizerInputDataConfig {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("EntityTypes" Core..= entityTypes),
            ("Annotations" Core..=) Core.<$> annotations,
            ("AugmentedManifests" Core..=) Core.<$> augmentedManifests,
            ("DataFormat" Core..=) Core.<$> dataFormat,
            ("Documents" Core..=) Core.<$> documents,
            ("EntityList" Core..=) Core.<$> entityList
          ]
      )

instance Core.FromJSON EntityRecognizerInputDataConfig where
  parseJSON =
    Core.withObject "EntityRecognizerInputDataConfig" Core.$
      \x ->
        EntityRecognizerInputDataConfig'
          Core.<$> (x Core..:? "EntityTypes" Core..!= Core.mempty)
          Core.<*> (x Core..:? "Annotations")
          Core.<*> (x Core..:? "AugmentedManifests")
          Core.<*> (x Core..:? "DataFormat")
          Core.<*> (x Core..:? "Documents")
          Core.<*> (x Core..:? "EntityList")
