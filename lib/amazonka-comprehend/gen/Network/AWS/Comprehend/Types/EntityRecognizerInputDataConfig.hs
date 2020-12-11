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
    eridcAugmentedManifests,
    eridcAnnotations,
    eridcDataFormat,
    eridcDocuments,
    eridcEntityList,
    eridcEntityTypes,
  )
where

import Network.AWS.Comprehend.Types.AugmentedManifestsListItem
import Network.AWS.Comprehend.Types.EntityRecognizerAnnotations
import Network.AWS.Comprehend.Types.EntityRecognizerDataFormat
import Network.AWS.Comprehend.Types.EntityRecognizerDocuments
import Network.AWS.Comprehend.Types.EntityRecognizerEntityList
import Network.AWS.Comprehend.Types.EntityTypesListItem
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the format and location of the input data.
--
-- /See:/ 'mkEntityRecognizerInputDataConfig' smart constructor.
data EntityRecognizerInputDataConfig = EntityRecognizerInputDataConfig'
  { augmentedManifests ::
      Lude.Maybe
        [AugmentedManifestsListItem],
    annotations ::
      Lude.Maybe
        EntityRecognizerAnnotations,
    dataFormat ::
      Lude.Maybe
        EntityRecognizerDataFormat,
    documents ::
      Lude.Maybe
        EntityRecognizerDocuments,
    entityList ::
      Lude.Maybe
        EntityRecognizerEntityList,
    entityTypes ::
      [EntityTypesListItem]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EntityRecognizerInputDataConfig' with the minimum fields required to make a request.
--
-- * 'annotations' - The S3 location of the CSV file that annotates your training documents.
-- * 'augmentedManifests' - A list of augmented manifest files that provide training data for your custom model. An augmented manifest file is a labeled dataset that is produced by Amazon SageMaker Ground Truth.
--
-- This parameter is required if you set @DataFormat@ to @AUGMENTED_MANIFEST@ .
-- * 'dataFormat' - The format of your training data:
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
-- * 'documents' - The S3 location of the folder that contains the training documents for your custom entity recognizer.
--
-- This parameter is required if you set @DataFormat@ to @COMPREHEND_CSV@ .
-- * 'entityList' - The S3 location of the CSV file that has the entity list for your custom entity recognizer.
-- * 'entityTypes' - The entity types in the labeled training data that Amazon Comprehend uses to train the custom entity recognizer. Any entity types that you don't specify are ignored.
--
-- A maximum of 25 entity types can be used at one time to train an entity recognizer. Entity types must not contain the following invalid characters: \n (line break), \\n (escaped line break), \r (carriage return), \\r (escaped carriage return), \t (tab), \\t (escaped tab), space, and , (comma).
mkEntityRecognizerInputDataConfig ::
  EntityRecognizerInputDataConfig
mkEntityRecognizerInputDataConfig =
  EntityRecognizerInputDataConfig'
    { augmentedManifests =
        Lude.Nothing,
      annotations = Lude.Nothing,
      dataFormat = Lude.Nothing,
      documents = Lude.Nothing,
      entityList = Lude.Nothing,
      entityTypes = Lude.mempty
    }

-- | A list of augmented manifest files that provide training data for your custom model. An augmented manifest file is a labeled dataset that is produced by Amazon SageMaker Ground Truth.
--
-- This parameter is required if you set @DataFormat@ to @AUGMENTED_MANIFEST@ .
--
-- /Note:/ Consider using 'augmentedManifests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eridcAugmentedManifests :: Lens.Lens' EntityRecognizerInputDataConfig (Lude.Maybe [AugmentedManifestsListItem])
eridcAugmentedManifests = Lens.lens (augmentedManifests :: EntityRecognizerInputDataConfig -> Lude.Maybe [AugmentedManifestsListItem]) (\s a -> s {augmentedManifests = a} :: EntityRecognizerInputDataConfig)
{-# DEPRECATED eridcAugmentedManifests "Use generic-lens or generic-optics with 'augmentedManifests' instead." #-}

-- | The S3 location of the CSV file that annotates your training documents.
--
-- /Note:/ Consider using 'annotations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eridcAnnotations :: Lens.Lens' EntityRecognizerInputDataConfig (Lude.Maybe EntityRecognizerAnnotations)
eridcAnnotations = Lens.lens (annotations :: EntityRecognizerInputDataConfig -> Lude.Maybe EntityRecognizerAnnotations) (\s a -> s {annotations = a} :: EntityRecognizerInputDataConfig)
{-# DEPRECATED eridcAnnotations "Use generic-lens or generic-optics with 'annotations' instead." #-}

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
eridcDataFormat :: Lens.Lens' EntityRecognizerInputDataConfig (Lude.Maybe EntityRecognizerDataFormat)
eridcDataFormat = Lens.lens (dataFormat :: EntityRecognizerInputDataConfig -> Lude.Maybe EntityRecognizerDataFormat) (\s a -> s {dataFormat = a} :: EntityRecognizerInputDataConfig)
{-# DEPRECATED eridcDataFormat "Use generic-lens or generic-optics with 'dataFormat' instead." #-}

-- | The S3 location of the folder that contains the training documents for your custom entity recognizer.
--
-- This parameter is required if you set @DataFormat@ to @COMPREHEND_CSV@ .
--
-- /Note:/ Consider using 'documents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eridcDocuments :: Lens.Lens' EntityRecognizerInputDataConfig (Lude.Maybe EntityRecognizerDocuments)
eridcDocuments = Lens.lens (documents :: EntityRecognizerInputDataConfig -> Lude.Maybe EntityRecognizerDocuments) (\s a -> s {documents = a} :: EntityRecognizerInputDataConfig)
{-# DEPRECATED eridcDocuments "Use generic-lens or generic-optics with 'documents' instead." #-}

-- | The S3 location of the CSV file that has the entity list for your custom entity recognizer.
--
-- /Note:/ Consider using 'entityList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eridcEntityList :: Lens.Lens' EntityRecognizerInputDataConfig (Lude.Maybe EntityRecognizerEntityList)
eridcEntityList = Lens.lens (entityList :: EntityRecognizerInputDataConfig -> Lude.Maybe EntityRecognizerEntityList) (\s a -> s {entityList = a} :: EntityRecognizerInputDataConfig)
{-# DEPRECATED eridcEntityList "Use generic-lens or generic-optics with 'entityList' instead." #-}

-- | The entity types in the labeled training data that Amazon Comprehend uses to train the custom entity recognizer. Any entity types that you don't specify are ignored.
--
-- A maximum of 25 entity types can be used at one time to train an entity recognizer. Entity types must not contain the following invalid characters: \n (line break), \\n (escaped line break), \r (carriage return), \\r (escaped carriage return), \t (tab), \\t (escaped tab), space, and , (comma).
--
-- /Note:/ Consider using 'entityTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eridcEntityTypes :: Lens.Lens' EntityRecognizerInputDataConfig [EntityTypesListItem]
eridcEntityTypes = Lens.lens (entityTypes :: EntityRecognizerInputDataConfig -> [EntityTypesListItem]) (\s a -> s {entityTypes = a} :: EntityRecognizerInputDataConfig)
{-# DEPRECATED eridcEntityTypes "Use generic-lens or generic-optics with 'entityTypes' instead." #-}

instance Lude.FromJSON EntityRecognizerInputDataConfig where
  parseJSON =
    Lude.withObject
      "EntityRecognizerInputDataConfig"
      ( \x ->
          EntityRecognizerInputDataConfig'
            Lude.<$> (x Lude..:? "AugmentedManifests" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Annotations")
            Lude.<*> (x Lude..:? "DataFormat")
            Lude.<*> (x Lude..:? "Documents")
            Lude.<*> (x Lude..:? "EntityList")
            Lude.<*> (x Lude..:? "EntityTypes" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON EntityRecognizerInputDataConfig where
  toJSON EntityRecognizerInputDataConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AugmentedManifests" Lude..=) Lude.<$> augmentedManifests,
            ("Annotations" Lude..=) Lude.<$> annotations,
            ("DataFormat" Lude..=) Lude.<$> dataFormat,
            ("Documents" Lude..=) Lude.<$> documents,
            ("EntityList" Lude..=) Lude.<$> entityList,
            Lude.Just ("EntityTypes" Lude..= entityTypes)
          ]
      )
