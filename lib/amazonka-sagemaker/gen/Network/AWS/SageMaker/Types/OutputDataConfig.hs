{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.OutputDataConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.OutputDataConfig
  ( OutputDataConfig (..),

    -- * Smart constructor
    mkOutputDataConfig,

    -- * Lenses
    odcS3OutputPath,
    odcKMSKeyId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides information about how to store model training results (model artifacts).
--
-- /See:/ 'mkOutputDataConfig' smart constructor.
data OutputDataConfig = OutputDataConfig'
  { -- | Identifies the S3 path where you want Amazon SageMaker to store the model artifacts. For example, @s3://bucket-name/key-name-prefix@ .
    s3OutputPath :: Lude.Text,
    -- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt the model artifacts at rest using Amazon S3 server-side encryption. The @KmsKeyId@ can be any of the following formats:
    --
    --
    --     * // KMS Key ID
    -- @"1234abcd-12ab-34cd-56ef-1234567890ab"@
    --
    --
    --     * // Amazon Resource Name (ARN) of a KMS Key
    -- @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
    --
    --
    --     * // KMS Key Alias
    -- @"alias/ExampleAlias"@
    --
    --
    --     * // Amazon Resource Name (ARN) of a KMS Key Alias
    -- @"arn:aws:kms:us-west-2:111122223333:alias/ExampleAlias"@
    --
    --
    -- If you use a KMS key ID or an alias of your master key, the Amazon SageMaker execution role must include permissions to call @kms:Encrypt@ . If you don't provide a KMS key ID, Amazon SageMaker uses the default KMS key for Amazon S3 for your role's account. Amazon SageMaker uses server-side encryption with KMS-managed keys for @OutputDataConfig@ . If you use a bucket policy with an @s3:PutObject@ permission that only allows objects with server-side encryption, set the condition key of @s3:x-amz-server-side-encryption@ to @"aws:kms"@ . For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html KMS-Managed Encryption Keys> in the /Amazon Simple Storage Service Developer Guide./
    -- The KMS key policy must grant permission to the IAM role that you specify in your @CreateTrainingJob@ , @CreateTransformJob@ , or @CreateHyperParameterTuningJob@ requests. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Using Key Policies in AWS KMS> in the /AWS Key Management Service Developer Guide/ .
    kmsKeyId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OutputDataConfig' with the minimum fields required to make a request.
--
-- * 's3OutputPath' - Identifies the S3 path where you want Amazon SageMaker to store the model artifacts. For example, @s3://bucket-name/key-name-prefix@ .
-- * 'kmsKeyId' - The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt the model artifacts at rest using Amazon S3 server-side encryption. The @KmsKeyId@ can be any of the following formats:
--
--
--     * // KMS Key ID
-- @"1234abcd-12ab-34cd-56ef-1234567890ab"@
--
--
--     * // Amazon Resource Name (ARN) of a KMS Key
-- @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
--
--
--     * // KMS Key Alias
-- @"alias/ExampleAlias"@
--
--
--     * // Amazon Resource Name (ARN) of a KMS Key Alias
-- @"arn:aws:kms:us-west-2:111122223333:alias/ExampleAlias"@
--
--
-- If you use a KMS key ID or an alias of your master key, the Amazon SageMaker execution role must include permissions to call @kms:Encrypt@ . If you don't provide a KMS key ID, Amazon SageMaker uses the default KMS key for Amazon S3 for your role's account. Amazon SageMaker uses server-side encryption with KMS-managed keys for @OutputDataConfig@ . If you use a bucket policy with an @s3:PutObject@ permission that only allows objects with server-side encryption, set the condition key of @s3:x-amz-server-side-encryption@ to @"aws:kms"@ . For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html KMS-Managed Encryption Keys> in the /Amazon Simple Storage Service Developer Guide./
-- The KMS key policy must grant permission to the IAM role that you specify in your @CreateTrainingJob@ , @CreateTransformJob@ , or @CreateHyperParameterTuningJob@ requests. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Using Key Policies in AWS KMS> in the /AWS Key Management Service Developer Guide/ .
mkOutputDataConfig ::
  -- | 's3OutputPath'
  Lude.Text ->
  OutputDataConfig
mkOutputDataConfig pS3OutputPath_ =
  OutputDataConfig'
    { s3OutputPath = pS3OutputPath_,
      kmsKeyId = Lude.Nothing
    }

-- | Identifies the S3 path where you want Amazon SageMaker to store the model artifacts. For example, @s3://bucket-name/key-name-prefix@ .
--
-- /Note:/ Consider using 's3OutputPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odcS3OutputPath :: Lens.Lens' OutputDataConfig Lude.Text
odcS3OutputPath = Lens.lens (s3OutputPath :: OutputDataConfig -> Lude.Text) (\s a -> s {s3OutputPath = a} :: OutputDataConfig)
{-# DEPRECATED odcS3OutputPath "Use generic-lens or generic-optics with 's3OutputPath' instead." #-}

-- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt the model artifacts at rest using Amazon S3 server-side encryption. The @KmsKeyId@ can be any of the following formats:
--
--
--     * // KMS Key ID
-- @"1234abcd-12ab-34cd-56ef-1234567890ab"@
--
--
--     * // Amazon Resource Name (ARN) of a KMS Key
-- @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@
--
--
--     * // KMS Key Alias
-- @"alias/ExampleAlias"@
--
--
--     * // Amazon Resource Name (ARN) of a KMS Key Alias
-- @"arn:aws:kms:us-west-2:111122223333:alias/ExampleAlias"@
--
--
-- If you use a KMS key ID or an alias of your master key, the Amazon SageMaker execution role must include permissions to call @kms:Encrypt@ . If you don't provide a KMS key ID, Amazon SageMaker uses the default KMS key for Amazon S3 for your role's account. Amazon SageMaker uses server-side encryption with KMS-managed keys for @OutputDataConfig@ . If you use a bucket policy with an @s3:PutObject@ permission that only allows objects with server-side encryption, set the condition key of @s3:x-amz-server-side-encryption@ to @"aws:kms"@ . For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html KMS-Managed Encryption Keys> in the /Amazon Simple Storage Service Developer Guide./
-- The KMS key policy must grant permission to the IAM role that you specify in your @CreateTrainingJob@ , @CreateTransformJob@ , or @CreateHyperParameterTuningJob@ requests. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Using Key Policies in AWS KMS> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odcKMSKeyId :: Lens.Lens' OutputDataConfig (Lude.Maybe Lude.Text)
odcKMSKeyId = Lens.lens (kmsKeyId :: OutputDataConfig -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: OutputDataConfig)
{-# DEPRECATED odcKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

instance Lude.FromJSON OutputDataConfig where
  parseJSON =
    Lude.withObject
      "OutputDataConfig"
      ( \x ->
          OutputDataConfig'
            Lude.<$> (x Lude..: "S3OutputPath") Lude.<*> (x Lude..:? "KmsKeyId")
      )

instance Lude.ToJSON OutputDataConfig where
  toJSON OutputDataConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("S3OutputPath" Lude..= s3OutputPath),
            ("KmsKeyId" Lude..=) Lude.<$> kmsKeyId
          ]
      )
