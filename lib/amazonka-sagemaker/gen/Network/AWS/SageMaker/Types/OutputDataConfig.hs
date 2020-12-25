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
    odcKmsKeyId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.KmsKeyId as Types
import qualified Network.AWS.SageMaker.Types.S3OutputPath as Types

-- | Provides information about how to store model training results (model artifacts).
--
-- /See:/ 'mkOutputDataConfig' smart constructor.
data OutputDataConfig = OutputDataConfig'
  { -- | Identifies the S3 path where you want Amazon SageMaker to store the model artifacts. For example, @s3://bucket-name/key-name-prefix@ .
    s3OutputPath :: Types.S3OutputPath,
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
    kmsKeyId :: Core.Maybe Types.KmsKeyId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OutputDataConfig' value with any optional fields omitted.
mkOutputDataConfig ::
  -- | 's3OutputPath'
  Types.S3OutputPath ->
  OutputDataConfig
mkOutputDataConfig s3OutputPath =
  OutputDataConfig' {s3OutputPath, kmsKeyId = Core.Nothing}

-- | Identifies the S3 path where you want Amazon SageMaker to store the model artifacts. For example, @s3://bucket-name/key-name-prefix@ .
--
-- /Note:/ Consider using 's3OutputPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
odcS3OutputPath :: Lens.Lens' OutputDataConfig Types.S3OutputPath
odcS3OutputPath = Lens.field @"s3OutputPath"
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
odcKmsKeyId :: Lens.Lens' OutputDataConfig (Core.Maybe Types.KmsKeyId)
odcKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED odcKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

instance Core.FromJSON OutputDataConfig where
  toJSON OutputDataConfig {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("S3OutputPath" Core..= s3OutputPath),
            ("KmsKeyId" Core..=) Core.<$> kmsKeyId
          ]
      )

instance Core.FromJSON OutputDataConfig where
  parseJSON =
    Core.withObject "OutputDataConfig" Core.$
      \x ->
        OutputDataConfig'
          Core.<$> (x Core..: "S3OutputPath") Core.<*> (x Core..:? "KmsKeyId")
