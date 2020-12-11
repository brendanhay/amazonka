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
    ljocSNSTopicARN,
    ljocKMSKeyId,
    ljocS3OutputPath,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Output configuration information for a labeling job.
--
-- /See:/ 'mkLabelingJobOutputConfig' smart constructor.
data LabelingJobOutputConfig = LabelingJobOutputConfig'
  { snsTopicARN ::
      Lude.Maybe Lude.Text,
    kmsKeyId :: Lude.Maybe Lude.Text,
    s3OutputPath :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LabelingJobOutputConfig' with the minimum fields required to make a request.
--
-- * 'kmsKeyId' - The AWS Key Management Service ID of the key used to encrypt the output data, if any.
--
-- If you use a KMS key ID or an alias of your master key, the Amazon SageMaker execution role must include permissions to call @kms:Encrypt@ . If you don't provide a KMS key ID, Amazon SageMaker uses the default KMS key for Amazon S3 for your role's account. Amazon SageMaker uses server-side encryption with KMS-managed keys for @LabelingJobOutputConfig@ . If you use a bucket policy with an @s3:PutObject@ permission that only allows objects with server-side encryption, set the condition key of @s3:x-amz-server-side-encryption@ to @"aws:kms"@ . For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html KMS-Managed Encryption Keys> in the /Amazon Simple Storage Service Developer Guide./
-- The KMS key policy must grant permission to the IAM role that you specify in your @CreateLabelingJob@ request. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Using Key Policies in AWS KMS> in the /AWS Key Management Service Developer Guide/ .
-- * 's3OutputPath' - The Amazon S3 location to write output data.
-- * 'snsTopicARN' - An Amazon Simple Notification Service (Amazon SNS) output topic ARN.
--
-- When workers complete labeling tasks, Ground Truth will send labeling task output data to the SNS output topic you specify here.
-- You must provide a value for this parameter if you provide an Amazon SNS input topic in @SnsDataSource@ in @InputConfig@ .
mkLabelingJobOutputConfig ::
  -- | 's3OutputPath'
  Lude.Text ->
  LabelingJobOutputConfig
mkLabelingJobOutputConfig pS3OutputPath_ =
  LabelingJobOutputConfig'
    { snsTopicARN = Lude.Nothing,
      kmsKeyId = Lude.Nothing,
      s3OutputPath = pS3OutputPath_
    }

-- | An Amazon Simple Notification Service (Amazon SNS) output topic ARN.
--
-- When workers complete labeling tasks, Ground Truth will send labeling task output data to the SNS output topic you specify here.
-- You must provide a value for this parameter if you provide an Amazon SNS input topic in @SnsDataSource@ in @InputConfig@ .
--
-- /Note:/ Consider using 'snsTopicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljocSNSTopicARN :: Lens.Lens' LabelingJobOutputConfig (Lude.Maybe Lude.Text)
ljocSNSTopicARN = Lens.lens (snsTopicARN :: LabelingJobOutputConfig -> Lude.Maybe Lude.Text) (\s a -> s {snsTopicARN = a} :: LabelingJobOutputConfig)
{-# DEPRECATED ljocSNSTopicARN "Use generic-lens or generic-optics with 'snsTopicARN' instead." #-}

-- | The AWS Key Management Service ID of the key used to encrypt the output data, if any.
--
-- If you use a KMS key ID or an alias of your master key, the Amazon SageMaker execution role must include permissions to call @kms:Encrypt@ . If you don't provide a KMS key ID, Amazon SageMaker uses the default KMS key for Amazon S3 for your role's account. Amazon SageMaker uses server-side encryption with KMS-managed keys for @LabelingJobOutputConfig@ . If you use a bucket policy with an @s3:PutObject@ permission that only allows objects with server-side encryption, set the condition key of @s3:x-amz-server-side-encryption@ to @"aws:kms"@ . For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html KMS-Managed Encryption Keys> in the /Amazon Simple Storage Service Developer Guide./
-- The KMS key policy must grant permission to the IAM role that you specify in your @CreateLabelingJob@ request. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Using Key Policies in AWS KMS> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljocKMSKeyId :: Lens.Lens' LabelingJobOutputConfig (Lude.Maybe Lude.Text)
ljocKMSKeyId = Lens.lens (kmsKeyId :: LabelingJobOutputConfig -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: LabelingJobOutputConfig)
{-# DEPRECATED ljocKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The Amazon S3 location to write output data.
--
-- /Note:/ Consider using 's3OutputPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ljocS3OutputPath :: Lens.Lens' LabelingJobOutputConfig Lude.Text
ljocS3OutputPath = Lens.lens (s3OutputPath :: LabelingJobOutputConfig -> Lude.Text) (\s a -> s {s3OutputPath = a} :: LabelingJobOutputConfig)
{-# DEPRECATED ljocS3OutputPath "Use generic-lens or generic-optics with 's3OutputPath' instead." #-}

instance Lude.FromJSON LabelingJobOutputConfig where
  parseJSON =
    Lude.withObject
      "LabelingJobOutputConfig"
      ( \x ->
          LabelingJobOutputConfig'
            Lude.<$> (x Lude..:? "SnsTopicArn")
            Lude.<*> (x Lude..:? "KmsKeyId")
            Lude.<*> (x Lude..: "S3OutputPath")
      )

instance Lude.ToJSON LabelingJobOutputConfig where
  toJSON LabelingJobOutputConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("SnsTopicArn" Lude..=) Lude.<$> snsTopicARN,
            ("KmsKeyId" Lude..=) Lude.<$> kmsKeyId,
            Lude.Just ("S3OutputPath" Lude..= s3OutputPath)
          ]
      )
