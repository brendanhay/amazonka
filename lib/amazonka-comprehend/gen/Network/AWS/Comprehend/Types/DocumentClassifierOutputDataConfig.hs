{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.DocumentClassifierOutputDataConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Comprehend.Types.DocumentClassifierOutputDataConfig
  ( DocumentClassifierOutputDataConfig (..)
  -- * Smart constructor
  , mkDocumentClassifierOutputDataConfig
  -- * Lenses
  , dcodcKmsKeyId
  , dcodcS3Uri
  ) where

import qualified Network.AWS.Comprehend.Types.KmsKeyId as Types
import qualified Network.AWS.Comprehend.Types.S3Uri as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides output results configuration parameters for custom classifier jobs. 
--
-- /See:/ 'mkDocumentClassifierOutputDataConfig' smart constructor.
data DocumentClassifierOutputDataConfig = DocumentClassifierOutputDataConfig'
  { kmsKeyId :: Core.Maybe Types.KmsKeyId
    -- ^ ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt the output results from an analysis job. The KmsKeyId can be one of the following formats:
--
--
--     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@ 
--
--
--     * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@ 
--
--
--     * KMS Key Alias: @"alias/ExampleAlias"@ 
--
--
--     * ARN of a KMS Key Alias: @"arn:aws:kms:us-west-2:111122223333:alias/ExampleAlias"@ 
--
--
  , s3Uri :: Core.Maybe Types.S3Uri
    -- ^ When you use the @OutputDataConfig@ object while creating a custom classifier, you specify the Amazon S3 location where you want to write the confusion matrix. The URI must be in the same region as the API endpoint that you are calling. The location is used as the prefix for the actual location of this output file.
--
-- When the custom classifier job is finished, the service creates the output file in a directory specific to the job. The @S3Uri@ field contains the location of the output file, called @output.tar.gz@ . It is a compressed archive that contains the confusion matrix.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DocumentClassifierOutputDataConfig' value with any optional fields omitted.
mkDocumentClassifierOutputDataConfig
    :: DocumentClassifierOutputDataConfig
mkDocumentClassifierOutputDataConfig
  = DocumentClassifierOutputDataConfig'{kmsKeyId = Core.Nothing,
                                        s3Uri = Core.Nothing}

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt the output results from an analysis job. The KmsKeyId can be one of the following formats:
--
--
--     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@ 
--
--
--     * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@ 
--
--
--     * KMS Key Alias: @"alias/ExampleAlias"@ 
--
--
--     * ARN of a KMS Key Alias: @"arn:aws:kms:us-west-2:111122223333:alias/ExampleAlias"@ 
--
--
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcodcKmsKeyId :: Lens.Lens' DocumentClassifierOutputDataConfig (Core.Maybe Types.KmsKeyId)
dcodcKmsKeyId = Lens.field @"kmsKeyId"
{-# INLINEABLE dcodcKmsKeyId #-}
{-# DEPRECATED kmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead"  #-}

-- | When you use the @OutputDataConfig@ object while creating a custom classifier, you specify the Amazon S3 location where you want to write the confusion matrix. The URI must be in the same region as the API endpoint that you are calling. The location is used as the prefix for the actual location of this output file.
--
-- When the custom classifier job is finished, the service creates the output file in a directory specific to the job. The @S3Uri@ field contains the location of the output file, called @output.tar.gz@ . It is a compressed archive that contains the confusion matrix.
--
-- /Note:/ Consider using 's3Uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcodcS3Uri :: Lens.Lens' DocumentClassifierOutputDataConfig (Core.Maybe Types.S3Uri)
dcodcS3Uri = Lens.field @"s3Uri"
{-# INLINEABLE dcodcS3Uri #-}
{-# DEPRECATED s3Uri "Use generic-lens or generic-optics with 's3Uri' instead"  #-}

instance Core.FromJSON DocumentClassifierOutputDataConfig where
        toJSON DocumentClassifierOutputDataConfig{..}
          = Core.object
              (Core.catMaybes
                 [("KmsKeyId" Core..=) Core.<$> kmsKeyId,
                  ("S3Uri" Core..=) Core.<$> s3Uri])

instance Core.FromJSON DocumentClassifierOutputDataConfig where
        parseJSON
          = Core.withObject "DocumentClassifierOutputDataConfig" Core.$
              \ x ->
                DocumentClassifierOutputDataConfig' Core.<$>
                  (x Core..:? "KmsKeyId") Core.<*> x Core..:? "S3Uri"
