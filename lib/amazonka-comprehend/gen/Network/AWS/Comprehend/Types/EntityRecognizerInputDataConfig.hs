{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.EntityRecognizerInputDataConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
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
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the format and location of the input data.
--
--
--
-- /See:/ 'entityRecognizerInputDataConfig' smart constructor.
data EntityRecognizerInputDataConfig = EntityRecognizerInputDataConfig'
  { _eridcAugmentedManifests ::
      !( Maybe
           [AugmentedManifestsListItem]
       ),
    _eridcAnnotations ::
      !( Maybe
           EntityRecognizerAnnotations
       ),
    _eridcDataFormat ::
      !( Maybe
           EntityRecognizerDataFormat
       ),
    _eridcDocuments ::
      !( Maybe
           EntityRecognizerDocuments
       ),
    _eridcEntityList ::
      !( Maybe
           EntityRecognizerEntityList
       ),
    _eridcEntityTypes ::
      ![EntityTypesListItem]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EntityRecognizerInputDataConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eridcAugmentedManifests' - A list of augmented manifest files that provide training data for your custom model. An augmented manifest file is a labeled dataset that is produced by Amazon SageMaker Ground Truth. This parameter is required if you set @DataFormat@ to @AUGMENTED_MANIFEST@ .
--
-- * 'eridcAnnotations' - The S3 location of the CSV file that annotates your training documents.
--
-- * 'eridcDataFormat' - The format of your training data:     * @COMPREHEND_CSV@ : A CSV file that supplements your training documents. The CSV file contains information about the custom entities that your trained model will detect. The required format of the file depends on whether you are providing annotations or an entity list. If you use this value, you must provide your CSV file by using either the @Annotations@ or @EntityList@ parameters. You must provide your training documents by using the @Documents@ parameter.     * @AUGMENTED_MANIFEST@ : A labeled dataset that is produced by Amazon SageMaker Ground Truth. This file is in JSON lines format. Each line is a complete JSON object that contains a training document and its labels. Each label annotates a named entity in the training document.  If you use this value, you must provide the @AugmentedManifests@ parameter in your request. If you don't specify a value, Amazon Comprehend uses @COMPREHEND_CSV@ as the default.
--
-- * 'eridcDocuments' - The S3 location of the folder that contains the training documents for your custom entity recognizer. This parameter is required if you set @DataFormat@ to @COMPREHEND_CSV@ .
--
-- * 'eridcEntityList' - The S3 location of the CSV file that has the entity list for your custom entity recognizer.
--
-- * 'eridcEntityTypes' - The entity types in the labeled training data that Amazon Comprehend uses to train the custom entity recognizer. Any entity types that you don't specify are ignored. A maximum of 25 entity types can be used at one time to train an entity recognizer. Entity types must not contain the following invalid characters: \n (line break), \\n (escaped line break), \r (carriage return), \\r (escaped carriage return), \t (tab), \\t (escaped tab), space, and , (comma).
entityRecognizerInputDataConfig ::
  EntityRecognizerInputDataConfig
entityRecognizerInputDataConfig =
  EntityRecognizerInputDataConfig'
    { _eridcAugmentedManifests =
        Nothing,
      _eridcAnnotations = Nothing,
      _eridcDataFormat = Nothing,
      _eridcDocuments = Nothing,
      _eridcEntityList = Nothing,
      _eridcEntityTypes = mempty
    }

-- | A list of augmented manifest files that provide training data for your custom model. An augmented manifest file is a labeled dataset that is produced by Amazon SageMaker Ground Truth. This parameter is required if you set @DataFormat@ to @AUGMENTED_MANIFEST@ .
eridcAugmentedManifests :: Lens' EntityRecognizerInputDataConfig [AugmentedManifestsListItem]
eridcAugmentedManifests = lens _eridcAugmentedManifests (\s a -> s {_eridcAugmentedManifests = a}) . _Default . _Coerce

-- | The S3 location of the CSV file that annotates your training documents.
eridcAnnotations :: Lens' EntityRecognizerInputDataConfig (Maybe EntityRecognizerAnnotations)
eridcAnnotations = lens _eridcAnnotations (\s a -> s {_eridcAnnotations = a})

-- | The format of your training data:     * @COMPREHEND_CSV@ : A CSV file that supplements your training documents. The CSV file contains information about the custom entities that your trained model will detect. The required format of the file depends on whether you are providing annotations or an entity list. If you use this value, you must provide your CSV file by using either the @Annotations@ or @EntityList@ parameters. You must provide your training documents by using the @Documents@ parameter.     * @AUGMENTED_MANIFEST@ : A labeled dataset that is produced by Amazon SageMaker Ground Truth. This file is in JSON lines format. Each line is a complete JSON object that contains a training document and its labels. Each label annotates a named entity in the training document.  If you use this value, you must provide the @AugmentedManifests@ parameter in your request. If you don't specify a value, Amazon Comprehend uses @COMPREHEND_CSV@ as the default.
eridcDataFormat :: Lens' EntityRecognizerInputDataConfig (Maybe EntityRecognizerDataFormat)
eridcDataFormat = lens _eridcDataFormat (\s a -> s {_eridcDataFormat = a})

-- | The S3 location of the folder that contains the training documents for your custom entity recognizer. This parameter is required if you set @DataFormat@ to @COMPREHEND_CSV@ .
eridcDocuments :: Lens' EntityRecognizerInputDataConfig (Maybe EntityRecognizerDocuments)
eridcDocuments = lens _eridcDocuments (\s a -> s {_eridcDocuments = a})

-- | The S3 location of the CSV file that has the entity list for your custom entity recognizer.
eridcEntityList :: Lens' EntityRecognizerInputDataConfig (Maybe EntityRecognizerEntityList)
eridcEntityList = lens _eridcEntityList (\s a -> s {_eridcEntityList = a})

-- | The entity types in the labeled training data that Amazon Comprehend uses to train the custom entity recognizer. Any entity types that you don't specify are ignored. A maximum of 25 entity types can be used at one time to train an entity recognizer. Entity types must not contain the following invalid characters: \n (line break), \\n (escaped line break), \r (carriage return), \\r (escaped carriage return), \t (tab), \\t (escaped tab), space, and , (comma).
eridcEntityTypes :: Lens' EntityRecognizerInputDataConfig [EntityTypesListItem]
eridcEntityTypes = lens _eridcEntityTypes (\s a -> s {_eridcEntityTypes = a}) . _Coerce

instance FromJSON EntityRecognizerInputDataConfig where
  parseJSON =
    withObject
      "EntityRecognizerInputDataConfig"
      ( \x ->
          EntityRecognizerInputDataConfig'
            <$> (x .:? "AugmentedManifests" .!= mempty)
            <*> (x .:? "Annotations")
            <*> (x .:? "DataFormat")
            <*> (x .:? "Documents")
            <*> (x .:? "EntityList")
            <*> (x .:? "EntityTypes" .!= mempty)
      )

instance Hashable EntityRecognizerInputDataConfig

instance NFData EntityRecognizerInputDataConfig

instance ToJSON EntityRecognizerInputDataConfig where
  toJSON EntityRecognizerInputDataConfig' {..} =
    object
      ( catMaybes
          [ ("AugmentedManifests" .=) <$> _eridcAugmentedManifests,
            ("Annotations" .=) <$> _eridcAnnotations,
            ("DataFormat" .=) <$> _eridcDataFormat,
            ("Documents" .=) <$> _eridcDocuments,
            ("EntityList" .=) <$> _eridcEntityList,
            Just ("EntityTypes" .= _eridcEntityTypes)
          ]
      )
