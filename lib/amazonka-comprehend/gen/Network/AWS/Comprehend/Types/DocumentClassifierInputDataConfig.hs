{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    dcidcS3Uri,
  )
where

import qualified Network.AWS.Comprehend.Types.AugmentedManifestsListItem as Types
import qualified Network.AWS.Comprehend.Types.DocumentClassifierDataFormat as Types
import qualified Network.AWS.Comprehend.Types.LabelDelimiter as Types
import qualified Network.AWS.Comprehend.Types.S3Uri as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The input properties for training a document classifier.
--
-- For more information on how the input file is formatted, see 'how-document-classification-training-data' .
--
-- /See:/ 'mkDocumentClassifierInputDataConfig' smart constructor.
data DocumentClassifierInputDataConfig = DocumentClassifierInputDataConfig'
  { -- | A list of augmented manifest files that provide training data for your custom model. An augmented manifest file is a labeled dataset that is produced by Amazon SageMaker Ground Truth.
    --
    -- This parameter is required if you set @DataFormat@ to @AUGMENTED_MANIFEST@ .
    augmentedManifests :: Core.Maybe [Types.AugmentedManifestsListItem],
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
    dataFormat :: Core.Maybe Types.DocumentClassifierDataFormat,
    -- | Indicates the delimiter used to separate each label for training a multi-label classifier. The default delimiter between labels is a pipe (|). You can use a different character as a delimiter (if it's an allowed character) by specifying it under Delimiter for labels. If the training documents use a delimiter other than the default or the delimiter you specify, the labels on that line will be combined to make a single unique label, such as LABELLABELLABEL.
    labelDelimiter :: Core.Maybe Types.LabelDelimiter,
    -- | The Amazon S3 URI for the input data. The S3 bucket must be in the same region as the API endpoint that you are calling. The URI can point to a single input file or it can provide the prefix for a collection of input files.
    --
    -- For example, if you use the URI @S3://bucketName/prefix@ , if the prefix is a single file, Amazon Comprehend uses that file as input. If more than one file begins with the prefix, Amazon Comprehend uses all of them as input.
    -- This parameter is required if you set @DataFormat@ to @COMPREHEND_CSV@ .
    s3Uri :: Core.Maybe Types.S3Uri
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DocumentClassifierInputDataConfig' value with any optional fields omitted.
mkDocumentClassifierInputDataConfig ::
  DocumentClassifierInputDataConfig
mkDocumentClassifierInputDataConfig =
  DocumentClassifierInputDataConfig'
    { augmentedManifests =
        Core.Nothing,
      dataFormat = Core.Nothing,
      labelDelimiter = Core.Nothing,
      s3Uri = Core.Nothing
    }

-- | A list of augmented manifest files that provide training data for your custom model. An augmented manifest file is a labeled dataset that is produced by Amazon SageMaker Ground Truth.
--
-- This parameter is required if you set @DataFormat@ to @AUGMENTED_MANIFEST@ .
--
-- /Note:/ Consider using 'augmentedManifests' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcidcAugmentedManifests :: Lens.Lens' DocumentClassifierInputDataConfig (Core.Maybe [Types.AugmentedManifestsListItem])
dcidcAugmentedManifests = Lens.field @"augmentedManifests"
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
dcidcDataFormat :: Lens.Lens' DocumentClassifierInputDataConfig (Core.Maybe Types.DocumentClassifierDataFormat)
dcidcDataFormat = Lens.field @"dataFormat"
{-# DEPRECATED dcidcDataFormat "Use generic-lens or generic-optics with 'dataFormat' instead." #-}

-- | Indicates the delimiter used to separate each label for training a multi-label classifier. The default delimiter between labels is a pipe (|). You can use a different character as a delimiter (if it's an allowed character) by specifying it under Delimiter for labels. If the training documents use a delimiter other than the default or the delimiter you specify, the labels on that line will be combined to make a single unique label, such as LABELLABELLABEL.
--
-- /Note:/ Consider using 'labelDelimiter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcidcLabelDelimiter :: Lens.Lens' DocumentClassifierInputDataConfig (Core.Maybe Types.LabelDelimiter)
dcidcLabelDelimiter = Lens.field @"labelDelimiter"
{-# DEPRECATED dcidcLabelDelimiter "Use generic-lens or generic-optics with 'labelDelimiter' instead." #-}

-- | The Amazon S3 URI for the input data. The S3 bucket must be in the same region as the API endpoint that you are calling. The URI can point to a single input file or it can provide the prefix for a collection of input files.
--
-- For example, if you use the URI @S3://bucketName/prefix@ , if the prefix is a single file, Amazon Comprehend uses that file as input. If more than one file begins with the prefix, Amazon Comprehend uses all of them as input.
-- This parameter is required if you set @DataFormat@ to @COMPREHEND_CSV@ .
--
-- /Note:/ Consider using 's3Uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcidcS3Uri :: Lens.Lens' DocumentClassifierInputDataConfig (Core.Maybe Types.S3Uri)
dcidcS3Uri = Lens.field @"s3Uri"
{-# DEPRECATED dcidcS3Uri "Use generic-lens or generic-optics with 's3Uri' instead." #-}

instance Core.FromJSON DocumentClassifierInputDataConfig where
  toJSON DocumentClassifierInputDataConfig {..} =
    Core.object
      ( Core.catMaybes
          [ ("AugmentedManifests" Core..=) Core.<$> augmentedManifests,
            ("DataFormat" Core..=) Core.<$> dataFormat,
            ("LabelDelimiter" Core..=) Core.<$> labelDelimiter,
            ("S3Uri" Core..=) Core.<$> s3Uri
          ]
      )

instance Core.FromJSON DocumentClassifierInputDataConfig where
  parseJSON =
    Core.withObject "DocumentClassifierInputDataConfig" Core.$
      \x ->
        DocumentClassifierInputDataConfig'
          Core.<$> (x Core..:? "AugmentedManifests")
          Core.<*> (x Core..:? "DataFormat")
          Core.<*> (x Core..:? "LabelDelimiter")
          Core.<*> (x Core..:? "S3Uri")
