{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.DocumentClassifierInputDataConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.DocumentClassifierInputDataConfig where

import Network.AWS.Comprehend.Types.AugmentedManifestsListItem
import Network.AWS.Comprehend.Types.DocumentClassifierDataFormat
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The input properties for training a document classifier.
--
--
-- For more information on how the input file is formatted, see 'how-document-classification-training-data' .
--
--
-- /See:/ 'documentClassifierInputDataConfig' smart constructor.
data DocumentClassifierInputDataConfig = DocumentClassifierInputDataConfig'
  { _dcidcAugmentedManifests ::
      !( Maybe
           [AugmentedManifestsListItem]
       ),
    _dcidcDataFormat ::
      !( Maybe
           DocumentClassifierDataFormat
       ),
    _dcidcLabelDelimiter ::
      !(Maybe Text),
    _dcidcS3URI ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DocumentClassifierInputDataConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcidcAugmentedManifests' - A list of augmented manifest files that provide training data for your custom model. An augmented manifest file is a labeled dataset that is produced by Amazon SageMaker Ground Truth. This parameter is required if you set @DataFormat@ to @AUGMENTED_MANIFEST@ .
--
-- * 'dcidcDataFormat' - The format of your training data:     * @COMPREHEND_CSV@ : A two-column CSV file, where labels are provided in the first column, and documents are provided in the second. If you use this value, you must provide the @S3Uri@ parameter in your request.     * @AUGMENTED_MANIFEST@ : A labeled dataset that is produced by Amazon SageMaker Ground Truth. This file is in JSON lines format. Each line is a complete JSON object that contains a training document and its associated labels.  If you use this value, you must provide the @AugmentedManifests@ parameter in your request. If you don't specify a value, Amazon Comprehend uses @COMPREHEND_CSV@ as the default.
--
-- * 'dcidcLabelDelimiter' - Indicates the delimiter used to separate each label for training a multi-label classifier. The default delimiter between labels is a pipe (|). You can use a different character as a delimiter (if it's an allowed character) by specifying it under Delimiter for labels. If the training documents use a delimiter other than the default or the delimiter you specify, the labels on that line will be combined to make a single unique label, such as LABELLABELLABEL.
--
-- * 'dcidcS3URI' - The Amazon S3 URI for the input data. The S3 bucket must be in the same region as the API endpoint that you are calling. The URI can point to a single input file or it can provide the prefix for a collection of input files. For example, if you use the URI @S3://bucketName/prefix@ , if the prefix is a single file, Amazon Comprehend uses that file as input. If more than one file begins with the prefix, Amazon Comprehend uses all of them as input. This parameter is required if you set @DataFormat@ to @COMPREHEND_CSV@ .
documentClassifierInputDataConfig ::
  DocumentClassifierInputDataConfig
documentClassifierInputDataConfig =
  DocumentClassifierInputDataConfig'
    { _dcidcAugmentedManifests =
        Nothing,
      _dcidcDataFormat = Nothing,
      _dcidcLabelDelimiter = Nothing,
      _dcidcS3URI = Nothing
    }

-- | A list of augmented manifest files that provide training data for your custom model. An augmented manifest file is a labeled dataset that is produced by Amazon SageMaker Ground Truth. This parameter is required if you set @DataFormat@ to @AUGMENTED_MANIFEST@ .
dcidcAugmentedManifests :: Lens' DocumentClassifierInputDataConfig [AugmentedManifestsListItem]
dcidcAugmentedManifests = lens _dcidcAugmentedManifests (\s a -> s {_dcidcAugmentedManifests = a}) . _Default . _Coerce

-- | The format of your training data:     * @COMPREHEND_CSV@ : A two-column CSV file, where labels are provided in the first column, and documents are provided in the second. If you use this value, you must provide the @S3Uri@ parameter in your request.     * @AUGMENTED_MANIFEST@ : A labeled dataset that is produced by Amazon SageMaker Ground Truth. This file is in JSON lines format. Each line is a complete JSON object that contains a training document and its associated labels.  If you use this value, you must provide the @AugmentedManifests@ parameter in your request. If you don't specify a value, Amazon Comprehend uses @COMPREHEND_CSV@ as the default.
dcidcDataFormat :: Lens' DocumentClassifierInputDataConfig (Maybe DocumentClassifierDataFormat)
dcidcDataFormat = lens _dcidcDataFormat (\s a -> s {_dcidcDataFormat = a})

-- | Indicates the delimiter used to separate each label for training a multi-label classifier. The default delimiter between labels is a pipe (|). You can use a different character as a delimiter (if it's an allowed character) by specifying it under Delimiter for labels. If the training documents use a delimiter other than the default or the delimiter you specify, the labels on that line will be combined to make a single unique label, such as LABELLABELLABEL.
dcidcLabelDelimiter :: Lens' DocumentClassifierInputDataConfig (Maybe Text)
dcidcLabelDelimiter = lens _dcidcLabelDelimiter (\s a -> s {_dcidcLabelDelimiter = a})

-- | The Amazon S3 URI for the input data. The S3 bucket must be in the same region as the API endpoint that you are calling. The URI can point to a single input file or it can provide the prefix for a collection of input files. For example, if you use the URI @S3://bucketName/prefix@ , if the prefix is a single file, Amazon Comprehend uses that file as input. If more than one file begins with the prefix, Amazon Comprehend uses all of them as input. This parameter is required if you set @DataFormat@ to @COMPREHEND_CSV@ .
dcidcS3URI :: Lens' DocumentClassifierInputDataConfig (Maybe Text)
dcidcS3URI = lens _dcidcS3URI (\s a -> s {_dcidcS3URI = a})

instance FromJSON DocumentClassifierInputDataConfig where
  parseJSON =
    withObject
      "DocumentClassifierInputDataConfig"
      ( \x ->
          DocumentClassifierInputDataConfig'
            <$> (x .:? "AugmentedManifests" .!= mempty)
            <*> (x .:? "DataFormat")
            <*> (x .:? "LabelDelimiter")
            <*> (x .:? "S3Uri")
      )

instance Hashable DocumentClassifierInputDataConfig

instance NFData DocumentClassifierInputDataConfig

instance ToJSON DocumentClassifierInputDataConfig where
  toJSON DocumentClassifierInputDataConfig' {..} =
    object
      ( catMaybes
          [ ("AugmentedManifests" .=) <$> _dcidcAugmentedManifests,
            ("DataFormat" .=) <$> _dcidcDataFormat,
            ("LabelDelimiter" .=) <$> _dcidcLabelDelimiter,
            ("S3Uri" .=) <$> _dcidcS3URI
          ]
      )
