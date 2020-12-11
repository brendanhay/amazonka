-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.DocumentClassifierInputDataConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.DocumentClassifierInputDataConfig
  ( DocumentClassifierInputDataConfig (..),

    -- * Smart constructor
    mkDocumentClassifierInputDataConfig,

    -- * Lenses
    dcidcAugmentedManifests,
    dcidcDataFormat,
    dcidcLabelDelimiter,
    dcidcS3URI,
  )
where

import Network.AWS.Comprehend.Types.AugmentedManifestsListItem
import Network.AWS.Comprehend.Types.DocumentClassifierDataFormat
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The input properties for training a document classifier.
--
-- For more information on how the input file is formatted, see 'how-document-classification-training-data' .
--
-- /See:/ 'mkDocumentClassifierInputDataConfig' smart constructor.
data DocumentClassifierInputDataConfig = DocumentClassifierInputDataConfig'
  { augmentedManifests ::
      Lude.Maybe
        [AugmentedManifestsListItem],
    dataFormat ::
      Lude.Maybe
        DocumentClassifierDataFormat,
    labelDelimiter ::
      Lude.Maybe Lude.Text,
    s3URI ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DocumentClassifierInputDataConfig' with the minimum fields required to make a request.
--
-- * 'augmentedManifests' - A list of augmented manifest files that provide training data for your custom model. An augmented manifest file is a labeled dataset that is produced by Amazon SageMaker Ground Truth.
--
-- This parameter is required if you set @DataFormat@ to @AUGMENTED_MANIFEST@ .
-- * 'dataFormat' - The format of your training data:
--
--
--     * @COMPREHEND_CSV@ : A two-column CSV file, where labels are provided in the first column, and documents are provided in the second. If you use this value, you must provide the @S3Uri@ parameter in your request.
--
--
--     * @AUGMENTED_MANIFEST@ : A labeled dataset that is produced by Amazon SageMaker Ground Truth. This file is in JSON lines format. Each line is a complete JSON object that contains a training document and its associated labels.
-- If you use this value, you must provide the @AugmentedManifests@ parameter in your request.
--
--
-- If you don't specify a value, Amazon Comprehend uses @COMPREHEND_CSV@ as the default.
-- * 'labelDelimiter' - Indicates the delimiter used to separate each label for training a multi-label classifier. The default delimiter between labels is a pipe (|). You can use a different character as a delimiter (if it's an allowed character) by specifying it under Delimiter for labels. If the training documents use a delimiter other than the default or the delimiter you specify, the labels on that line will be combined to make a single unique label, such as LABELLABELLABEL.
-- * 's3URI' - The Amazon S3 URI for the input data. The S3 bucket must be in the same region as the API endpoint that you are calling. The URI can point to a single input file or it can provide the prefix for a collection of input files.
--
-- For example, if you use the URI @S3://bucketName/prefix@ , if the prefix is a single file, Amazon Comprehend uses that file as input. If more than one file begins with the prefix, Amazon Comprehend uses all of them as input.
-- This parameter is required if you set @DataFormat@ to @COMPREHEND_CSV@ .
mkDocumentClassifierInputDataConfig ::
  DocumentClassifierInputDataConfig
mkDocumentClassifierInputDataConfig =
  DocumentClassifierInputDataConfig'
    { augmentedManifests =
        Lude.Nothing,
      dataFormat = Lude.Nothing,
      labelDelimiter = Lude.Nothing,
      s3URI = Lude.Nothing
    }

-- | A list of augmented manifest files that provide training data for your custom model. An augmented manifest file is a labeled dataset that is produced by Amazon SageMaker Ground Truth.
--
-- This parameter is required if you set @DataFormat@ to @AUGMENTED_MANIFEST@ .
--
-- /Note:/ Consider using 'augmentedManifests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcidcAugmentedManifests :: Lens.Lens' DocumentClassifierInputDataConfig (Lude.Maybe [AugmentedManifestsListItem])
dcidcAugmentedManifests = Lens.lens (augmentedManifests :: DocumentClassifierInputDataConfig -> Lude.Maybe [AugmentedManifestsListItem]) (\s a -> s {augmentedManifests = a} :: DocumentClassifierInputDataConfig)
{-# DEPRECATED dcidcAugmentedManifests "Use generic-lens or generic-optics with 'augmentedManifests' instead." #-}

-- | The format of your training data:
--
--
--     * @COMPREHEND_CSV@ : A two-column CSV file, where labels are provided in the first column, and documents are provided in the second. If you use this value, you must provide the @S3Uri@ parameter in your request.
--
--
--     * @AUGMENTED_MANIFEST@ : A labeled dataset that is produced by Amazon SageMaker Ground Truth. This file is in JSON lines format. Each line is a complete JSON object that contains a training document and its associated labels.
-- If you use this value, you must provide the @AugmentedManifests@ parameter in your request.
--
--
-- If you don't specify a value, Amazon Comprehend uses @COMPREHEND_CSV@ as the default.
--
-- /Note:/ Consider using 'dataFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcidcDataFormat :: Lens.Lens' DocumentClassifierInputDataConfig (Lude.Maybe DocumentClassifierDataFormat)
dcidcDataFormat = Lens.lens (dataFormat :: DocumentClassifierInputDataConfig -> Lude.Maybe DocumentClassifierDataFormat) (\s a -> s {dataFormat = a} :: DocumentClassifierInputDataConfig)
{-# DEPRECATED dcidcDataFormat "Use generic-lens or generic-optics with 'dataFormat' instead." #-}

-- | Indicates the delimiter used to separate each label for training a multi-label classifier. The default delimiter between labels is a pipe (|). You can use a different character as a delimiter (if it's an allowed character) by specifying it under Delimiter for labels. If the training documents use a delimiter other than the default or the delimiter you specify, the labels on that line will be combined to make a single unique label, such as LABELLABELLABEL.
--
-- /Note:/ Consider using 'labelDelimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcidcLabelDelimiter :: Lens.Lens' DocumentClassifierInputDataConfig (Lude.Maybe Lude.Text)
dcidcLabelDelimiter = Lens.lens (labelDelimiter :: DocumentClassifierInputDataConfig -> Lude.Maybe Lude.Text) (\s a -> s {labelDelimiter = a} :: DocumentClassifierInputDataConfig)
{-# DEPRECATED dcidcLabelDelimiter "Use generic-lens or generic-optics with 'labelDelimiter' instead." #-}

-- | The Amazon S3 URI for the input data. The S3 bucket must be in the same region as the API endpoint that you are calling. The URI can point to a single input file or it can provide the prefix for a collection of input files.
--
-- For example, if you use the URI @S3://bucketName/prefix@ , if the prefix is a single file, Amazon Comprehend uses that file as input. If more than one file begins with the prefix, Amazon Comprehend uses all of them as input.
-- This parameter is required if you set @DataFormat@ to @COMPREHEND_CSV@ .
--
-- /Note:/ Consider using 's3URI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcidcS3URI :: Lens.Lens' DocumentClassifierInputDataConfig (Lude.Maybe Lude.Text)
dcidcS3URI = Lens.lens (s3URI :: DocumentClassifierInputDataConfig -> Lude.Maybe Lude.Text) (\s a -> s {s3URI = a} :: DocumentClassifierInputDataConfig)
{-# DEPRECATED dcidcS3URI "Use generic-lens or generic-optics with 's3URI' instead." #-}

instance Lude.FromJSON DocumentClassifierInputDataConfig where
  parseJSON =
    Lude.withObject
      "DocumentClassifierInputDataConfig"
      ( \x ->
          DocumentClassifierInputDataConfig'
            Lude.<$> (x Lude..:? "AugmentedManifests" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "DataFormat")
            Lude.<*> (x Lude..:? "LabelDelimiter")
            Lude.<*> (x Lude..:? "S3Uri")
      )

instance Lude.ToJSON DocumentClassifierInputDataConfig where
  toJSON DocumentClassifierInputDataConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AugmentedManifests" Lude..=) Lude.<$> augmentedManifests,
            ("DataFormat" Lude..=) Lude.<$> dataFormat,
            ("LabelDelimiter" Lude..=) Lude.<$> labelDelimiter,
            ("S3Uri" Lude..=) Lude.<$> s3URI
          ]
      )
