{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.LabelingJobOutputConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.LabelingJobOutputConfig where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Output configuration information for a labeling job.
--
-- /See:/ 'newLabelingJobOutputConfig' smart constructor.
data LabelingJobOutputConfig = LabelingJobOutputConfig'
  { -- | The AWS Key Management Service ID of the key used to encrypt the output
    -- data, if any.
    --
    -- If you use a KMS key ID or an alias of your master key, the Amazon
    -- SageMaker execution role must include permissions to call @kms:Encrypt@.
    -- If you don\'t provide a KMS key ID, Amazon SageMaker uses the default
    -- KMS key for Amazon S3 for your role\'s account. Amazon SageMaker uses
    -- server-side encryption with KMS-managed keys for
    -- @LabelingJobOutputConfig@. If you use a bucket policy with an
    -- @s3:PutObject@ permission that only allows objects with server-side
    -- encryption, set the condition key of @s3:x-amz-server-side-encryption@
    -- to @\"aws:kms\"@. For more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html KMS-Managed Encryption Keys>
    -- in the /Amazon Simple Storage Service Developer Guide./
    --
    -- The KMS key policy must grant permission to the IAM role that you
    -- specify in your @CreateLabelingJob@ request. For more information, see
    -- <http://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Using Key Policies in AWS KMS>
    -- in the /AWS Key Management Service Developer Guide/.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | An Amazon Simple Notification Service (Amazon SNS) output topic ARN.
    --
    -- When workers complete labeling tasks, Ground Truth will send labeling
    -- task output data to the SNS output topic you specify here.
    --
    -- You must provide a value for this parameter if you provide an Amazon SNS
    -- input topic in @SnsDataSource@ in @InputConfig@.
    snsTopicArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 location to write output data.
    s3OutputPath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LabelingJobOutputConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'labelingJobOutputConfig_kmsKeyId' - The AWS Key Management Service ID of the key used to encrypt the output
-- data, if any.
--
-- If you use a KMS key ID or an alias of your master key, the Amazon
-- SageMaker execution role must include permissions to call @kms:Encrypt@.
-- If you don\'t provide a KMS key ID, Amazon SageMaker uses the default
-- KMS key for Amazon S3 for your role\'s account. Amazon SageMaker uses
-- server-side encryption with KMS-managed keys for
-- @LabelingJobOutputConfig@. If you use a bucket policy with an
-- @s3:PutObject@ permission that only allows objects with server-side
-- encryption, set the condition key of @s3:x-amz-server-side-encryption@
-- to @\"aws:kms\"@. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html KMS-Managed Encryption Keys>
-- in the /Amazon Simple Storage Service Developer Guide./
--
-- The KMS key policy must grant permission to the IAM role that you
-- specify in your @CreateLabelingJob@ request. For more information, see
-- <http://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Using Key Policies in AWS KMS>
-- in the /AWS Key Management Service Developer Guide/.
--
-- 'snsTopicArn', 'labelingJobOutputConfig_snsTopicArn' - An Amazon Simple Notification Service (Amazon SNS) output topic ARN.
--
-- When workers complete labeling tasks, Ground Truth will send labeling
-- task output data to the SNS output topic you specify here.
--
-- You must provide a value for this parameter if you provide an Amazon SNS
-- input topic in @SnsDataSource@ in @InputConfig@.
--
-- 's3OutputPath', 'labelingJobOutputConfig_s3OutputPath' - The Amazon S3 location to write output data.
newLabelingJobOutputConfig ::
  -- | 's3OutputPath'
  Prelude.Text ->
  LabelingJobOutputConfig
newLabelingJobOutputConfig pS3OutputPath_ =
  LabelingJobOutputConfig'
    { kmsKeyId =
        Prelude.Nothing,
      snsTopicArn = Prelude.Nothing,
      s3OutputPath = pS3OutputPath_
    }

-- | The AWS Key Management Service ID of the key used to encrypt the output
-- data, if any.
--
-- If you use a KMS key ID or an alias of your master key, the Amazon
-- SageMaker execution role must include permissions to call @kms:Encrypt@.
-- If you don\'t provide a KMS key ID, Amazon SageMaker uses the default
-- KMS key for Amazon S3 for your role\'s account. Amazon SageMaker uses
-- server-side encryption with KMS-managed keys for
-- @LabelingJobOutputConfig@. If you use a bucket policy with an
-- @s3:PutObject@ permission that only allows objects with server-side
-- encryption, set the condition key of @s3:x-amz-server-side-encryption@
-- to @\"aws:kms\"@. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html KMS-Managed Encryption Keys>
-- in the /Amazon Simple Storage Service Developer Guide./
--
-- The KMS key policy must grant permission to the IAM role that you
-- specify in your @CreateLabelingJob@ request. For more information, see
-- <http://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Using Key Policies in AWS KMS>
-- in the /AWS Key Management Service Developer Guide/.
labelingJobOutputConfig_kmsKeyId :: Lens.Lens' LabelingJobOutputConfig (Prelude.Maybe Prelude.Text)
labelingJobOutputConfig_kmsKeyId = Lens.lens (\LabelingJobOutputConfig' {kmsKeyId} -> kmsKeyId) (\s@LabelingJobOutputConfig' {} a -> s {kmsKeyId = a} :: LabelingJobOutputConfig)

-- | An Amazon Simple Notification Service (Amazon SNS) output topic ARN.
--
-- When workers complete labeling tasks, Ground Truth will send labeling
-- task output data to the SNS output topic you specify here.
--
-- You must provide a value for this parameter if you provide an Amazon SNS
-- input topic in @SnsDataSource@ in @InputConfig@.
labelingJobOutputConfig_snsTopicArn :: Lens.Lens' LabelingJobOutputConfig (Prelude.Maybe Prelude.Text)
labelingJobOutputConfig_snsTopicArn = Lens.lens (\LabelingJobOutputConfig' {snsTopicArn} -> snsTopicArn) (\s@LabelingJobOutputConfig' {} a -> s {snsTopicArn = a} :: LabelingJobOutputConfig)

-- | The Amazon S3 location to write output data.
labelingJobOutputConfig_s3OutputPath :: Lens.Lens' LabelingJobOutputConfig Prelude.Text
labelingJobOutputConfig_s3OutputPath = Lens.lens (\LabelingJobOutputConfig' {s3OutputPath} -> s3OutputPath) (\s@LabelingJobOutputConfig' {} a -> s {s3OutputPath = a} :: LabelingJobOutputConfig)

instance Prelude.FromJSON LabelingJobOutputConfig where
  parseJSON =
    Prelude.withObject
      "LabelingJobOutputConfig"
      ( \x ->
          LabelingJobOutputConfig'
            Prelude.<$> (x Prelude..:? "KmsKeyId")
            Prelude.<*> (x Prelude..:? "SnsTopicArn")
            Prelude.<*> (x Prelude..: "S3OutputPath")
      )

instance Prelude.Hashable LabelingJobOutputConfig

instance Prelude.NFData LabelingJobOutputConfig

instance Prelude.ToJSON LabelingJobOutputConfig where
  toJSON LabelingJobOutputConfig' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("KmsKeyId" Prelude..=) Prelude.<$> kmsKeyId,
            ("SnsTopicArn" Prelude..=) Prelude.<$> snsTopicArn,
            Prelude.Just
              ("S3OutputPath" Prelude..= s3OutputPath)
          ]
      )
