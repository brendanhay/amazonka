{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.LabelingJobOutputConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.LabelingJobOutputConfig
  ( LabelingJobOutputConfig (..),

    -- * Smart constructor
    mkLabelingJobOutputConfig,

    -- * Lenses
    ljocS3OutputPath,
    ljocKmsKeyId,
    ljocSnsTopicArn,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.KmsKeyId as Types
import qualified Network.AWS.SageMaker.Types.S3OutputPath as Types
import qualified Network.AWS.SageMaker.Types.SnsTopicArn as Types

-- | Output configuration information for a labeling job.
--
-- /See:/ 'mkLabelingJobOutputConfig' smart constructor.
data LabelingJobOutputConfig = LabelingJobOutputConfig'
  { -- | The Amazon S3 location to write output data.
    s3OutputPath :: Types.S3OutputPath,
    -- | The AWS Key Management Service ID of the key used to encrypt the output data, if any.
    --
    -- If you use a KMS key ID or an alias of your master key, the Amazon SageMaker execution role must include permissions to call @kms:Encrypt@ . If you don't provide a KMS key ID, Amazon SageMaker uses the default KMS key for Amazon S3 for your role's account. Amazon SageMaker uses server-side encryption with KMS-managed keys for @LabelingJobOutputConfig@ . If you use a bucket policy with an @s3:PutObject@ permission that only allows objects with server-side encryption, set the condition key of @s3:x-amz-server-side-encryption@ to @"aws:kms"@ . For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html KMS-Managed Encryption Keys> in the /Amazon Simple Storage Service Developer Guide./
    -- The KMS key policy must grant permission to the IAM role that you specify in your @CreateLabelingJob@ request. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Using Key Policies in AWS KMS> in the /AWS Key Management Service Developer Guide/ .
    kmsKeyId :: Core.Maybe Types.KmsKeyId,
    -- | An Amazon Simple Notification Service (Amazon SNS) output topic ARN.
    --
    -- When workers complete labeling tasks, Ground Truth will send labeling task output data to the SNS output topic you specify here.
    -- You must provide a value for this parameter if you provide an Amazon SNS input topic in @SnsDataSource@ in @InputConfig@ .
    snsTopicArn :: Core.Maybe Types.SnsTopicArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LabelingJobOutputConfig' value with any optional fields omitted.
mkLabelingJobOutputConfig ::
  -- | 's3OutputPath'
  Types.S3OutputPath ->
  LabelingJobOutputConfig
mkLabelingJobOutputConfig s3OutputPath =
  LabelingJobOutputConfig'
    { s3OutputPath,
      kmsKeyId = Core.Nothing,
      snsTopicArn = Core.Nothing
    }

-- | The Amazon S3 location to write output data.
--
-- /Note:/ Consider using 's3OutputPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljocS3OutputPath :: Lens.Lens' LabelingJobOutputConfig Types.S3OutputPath
ljocS3OutputPath = Lens.field @"s3OutputPath"
{-# DEPRECATED ljocS3OutputPath "Use generic-lens or generic-optics with 's3OutputPath' instead." #-}

-- | The AWS Key Management Service ID of the key used to encrypt the output data, if any.
--
-- If you use a KMS key ID or an alias of your master key, the Amazon SageMaker execution role must include permissions to call @kms:Encrypt@ . If you don't provide a KMS key ID, Amazon SageMaker uses the default KMS key for Amazon S3 for your role's account. Amazon SageMaker uses server-side encryption with KMS-managed keys for @LabelingJobOutputConfig@ . If you use a bucket policy with an @s3:PutObject@ permission that only allows objects with server-side encryption, set the condition key of @s3:x-amz-server-side-encryption@ to @"aws:kms"@ . For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html KMS-Managed Encryption Keys> in the /Amazon Simple Storage Service Developer Guide./
-- The KMS key policy must grant permission to the IAM role that you specify in your @CreateLabelingJob@ request. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Using Key Policies in AWS KMS> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljocKmsKeyId :: Lens.Lens' LabelingJobOutputConfig (Core.Maybe Types.KmsKeyId)
ljocKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED ljocKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | An Amazon Simple Notification Service (Amazon SNS) output topic ARN.
--
-- When workers complete labeling tasks, Ground Truth will send labeling task output data to the SNS output topic you specify here.
-- You must provide a value for this parameter if you provide an Amazon SNS input topic in @SnsDataSource@ in @InputConfig@ .
--
-- /Note:/ Consider using 'snsTopicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljocSnsTopicArn :: Lens.Lens' LabelingJobOutputConfig (Core.Maybe Types.SnsTopicArn)
ljocSnsTopicArn = Lens.field @"snsTopicArn"
{-# DEPRECATED ljocSnsTopicArn "Use generic-lens or generic-optics with 'snsTopicArn' instead." #-}

instance Core.FromJSON LabelingJobOutputConfig where
  toJSON LabelingJobOutputConfig {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("S3OutputPath" Core..= s3OutputPath),
            ("KmsKeyId" Core..=) Core.<$> kmsKeyId,
            ("SnsTopicArn" Core..=) Core.<$> snsTopicArn
          ]
      )

instance Core.FromJSON LabelingJobOutputConfig where
  parseJSON =
    Core.withObject "LabelingJobOutputConfig" Core.$
      \x ->
        LabelingJobOutputConfig'
          Core.<$> (x Core..: "S3OutputPath")
          Core.<*> (x Core..:? "KmsKeyId")
          Core.<*> (x Core..:? "SnsTopicArn")
