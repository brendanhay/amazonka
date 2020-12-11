-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.DocumentClassifierOutputDataConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.DocumentClassifierOutputDataConfig
  ( DocumentClassifierOutputDataConfig (..),

    -- * Smart constructor
    mkDocumentClassifierOutputDataConfig,

    -- * Lenses
    dcodcKMSKeyId,
    dcodcS3URI,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides output results configuration parameters for custom classifier jobs.
--
-- /See:/ 'mkDocumentClassifierOutputDataConfig' smart constructor.
data DocumentClassifierOutputDataConfig = DocumentClassifierOutputDataConfig'
  { kmsKeyId ::
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

-- | Creates a value of 'DocumentClassifierOutputDataConfig' with the minimum fields required to make a request.
--
-- * 'kmsKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt the output results from an analysis job. The KmsKeyId can be one of the following formats:
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
-- * 's3URI' - When you use the @OutputDataConfig@ object while creating a custom classifier, you specify the Amazon S3 location where you want to write the confusion matrix. The URI must be in the same region as the API endpoint that you are calling. The location is used as the prefix for the actual location of this output file.
--
-- When the custom classifier job is finished, the service creates the output file in a directory specific to the job. The @S3Uri@ field contains the location of the output file, called @output.tar.gz@ . It is a compressed archive that contains the confusion matrix.
mkDocumentClassifierOutputDataConfig ::
  DocumentClassifierOutputDataConfig
mkDocumentClassifierOutputDataConfig =
  DocumentClassifierOutputDataConfig'
    { kmsKeyId = Lude.Nothing,
      s3URI = Lude.Nothing
    }

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
dcodcKMSKeyId :: Lens.Lens' DocumentClassifierOutputDataConfig (Lude.Maybe Lude.Text)
dcodcKMSKeyId = Lens.lens (kmsKeyId :: DocumentClassifierOutputDataConfig -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: DocumentClassifierOutputDataConfig)
{-# DEPRECATED dcodcKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | When you use the @OutputDataConfig@ object while creating a custom classifier, you specify the Amazon S3 location where you want to write the confusion matrix. The URI must be in the same region as the API endpoint that you are calling. The location is used as the prefix for the actual location of this output file.
--
-- When the custom classifier job is finished, the service creates the output file in a directory specific to the job. The @S3Uri@ field contains the location of the output file, called @output.tar.gz@ . It is a compressed archive that contains the confusion matrix.
--
-- /Note:/ Consider using 's3URI' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcodcS3URI :: Lens.Lens' DocumentClassifierOutputDataConfig (Lude.Maybe Lude.Text)
dcodcS3URI = Lens.lens (s3URI :: DocumentClassifierOutputDataConfig -> Lude.Maybe Lude.Text) (\s a -> s {s3URI = a} :: DocumentClassifierOutputDataConfig)
{-# DEPRECATED dcodcS3URI "Use generic-lens or generic-optics with 's3URI' instead." #-}

instance Lude.FromJSON DocumentClassifierOutputDataConfig where
  parseJSON =
    Lude.withObject
      "DocumentClassifierOutputDataConfig"
      ( \x ->
          DocumentClassifierOutputDataConfig'
            Lude.<$> (x Lude..:? "KmsKeyId") Lude.<*> (x Lude..:? "S3Uri")
      )

instance Lude.ToJSON DocumentClassifierOutputDataConfig where
  toJSON DocumentClassifierOutputDataConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("KmsKeyId" Lude..=) Lude.<$> kmsKeyId,
            ("S3Uri" Lude..=) Lude.<$> s3URI
          ]
      )
