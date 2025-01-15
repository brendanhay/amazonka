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
-- Module      : Amazonka.SageMaker.Types.LabelingJobOutputConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.LabelingJobOutputConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Output configuration information for a labeling job.
--
-- /See:/ 'newLabelingJobOutputConfig' smart constructor.
data LabelingJobOutputConfig = LabelingJobOutputConfig'
  { -- | The Amazon Web Services Key Management Service ID of the key used to
    -- encrypt the output data, if any.
    --
    -- If you provide your own KMS key ID, you must add the required
    -- permissions to your KMS key described in
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-security-permission.html#sms-security-kms-permissions Encrypt Output Data and Storage Volume with Amazon Web Services KMS>.
    --
    -- If you don\'t provide a KMS key ID, Amazon SageMaker uses the default
    -- Amazon Web Services KMS key for Amazon S3 for your role\'s account to
    -- encrypt your output data.
    --
    -- If you use a bucket policy with an @s3:PutObject@ permission that only
    -- allows objects with server-side encryption, set the condition key of
    -- @s3:x-amz-server-side-encryption@ to @\"aws:kms\"@. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html KMS-Managed Encryption Keys>
    -- in the /Amazon Simple Storage Service Developer Guide./
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | An Amazon Simple Notification Service (Amazon SNS) output topic ARN.
    -- Provide a @SnsTopicArn@ if you want to do real time chaining to another
    -- streaming job and receive an Amazon SNS notifications each time a data
    -- object is submitted by a worker.
    --
    -- If you provide an @SnsTopicArn@ in @OutputConfig@, when workers complete
    -- labeling tasks, Ground Truth will send labeling task output data to the
    -- SNS output topic you specify here.
    --
    -- To learn more, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-streaming-labeling-job.html#sms-streaming-how-it-works-output-data Receive Output Data from a Streaming Labeling Job>.
    snsTopicArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 location to write output data.
    s3OutputPath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LabelingJobOutputConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyId', 'labelingJobOutputConfig_kmsKeyId' - The Amazon Web Services Key Management Service ID of the key used to
-- encrypt the output data, if any.
--
-- If you provide your own KMS key ID, you must add the required
-- permissions to your KMS key described in
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-security-permission.html#sms-security-kms-permissions Encrypt Output Data and Storage Volume with Amazon Web Services KMS>.
--
-- If you don\'t provide a KMS key ID, Amazon SageMaker uses the default
-- Amazon Web Services KMS key for Amazon S3 for your role\'s account to
-- encrypt your output data.
--
-- If you use a bucket policy with an @s3:PutObject@ permission that only
-- allows objects with server-side encryption, set the condition key of
-- @s3:x-amz-server-side-encryption@ to @\"aws:kms\"@. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html KMS-Managed Encryption Keys>
-- in the /Amazon Simple Storage Service Developer Guide./
--
-- 'snsTopicArn', 'labelingJobOutputConfig_snsTopicArn' - An Amazon Simple Notification Service (Amazon SNS) output topic ARN.
-- Provide a @SnsTopicArn@ if you want to do real time chaining to another
-- streaming job and receive an Amazon SNS notifications each time a data
-- object is submitted by a worker.
--
-- If you provide an @SnsTopicArn@ in @OutputConfig@, when workers complete
-- labeling tasks, Ground Truth will send labeling task output data to the
-- SNS output topic you specify here.
--
-- To learn more, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-streaming-labeling-job.html#sms-streaming-how-it-works-output-data Receive Output Data from a Streaming Labeling Job>.
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

-- | The Amazon Web Services Key Management Service ID of the key used to
-- encrypt the output data, if any.
--
-- If you provide your own KMS key ID, you must add the required
-- permissions to your KMS key described in
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-security-permission.html#sms-security-kms-permissions Encrypt Output Data and Storage Volume with Amazon Web Services KMS>.
--
-- If you don\'t provide a KMS key ID, Amazon SageMaker uses the default
-- Amazon Web Services KMS key for Amazon S3 for your role\'s account to
-- encrypt your output data.
--
-- If you use a bucket policy with an @s3:PutObject@ permission that only
-- allows objects with server-side encryption, set the condition key of
-- @s3:x-amz-server-side-encryption@ to @\"aws:kms\"@. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html KMS-Managed Encryption Keys>
-- in the /Amazon Simple Storage Service Developer Guide./
labelingJobOutputConfig_kmsKeyId :: Lens.Lens' LabelingJobOutputConfig (Prelude.Maybe Prelude.Text)
labelingJobOutputConfig_kmsKeyId = Lens.lens (\LabelingJobOutputConfig' {kmsKeyId} -> kmsKeyId) (\s@LabelingJobOutputConfig' {} a -> s {kmsKeyId = a} :: LabelingJobOutputConfig)

-- | An Amazon Simple Notification Service (Amazon SNS) output topic ARN.
-- Provide a @SnsTopicArn@ if you want to do real time chaining to another
-- streaming job and receive an Amazon SNS notifications each time a data
-- object is submitted by a worker.
--
-- If you provide an @SnsTopicArn@ in @OutputConfig@, when workers complete
-- labeling tasks, Ground Truth will send labeling task output data to the
-- SNS output topic you specify here.
--
-- To learn more, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sms-streaming-labeling-job.html#sms-streaming-how-it-works-output-data Receive Output Data from a Streaming Labeling Job>.
labelingJobOutputConfig_snsTopicArn :: Lens.Lens' LabelingJobOutputConfig (Prelude.Maybe Prelude.Text)
labelingJobOutputConfig_snsTopicArn = Lens.lens (\LabelingJobOutputConfig' {snsTopicArn} -> snsTopicArn) (\s@LabelingJobOutputConfig' {} a -> s {snsTopicArn = a} :: LabelingJobOutputConfig)

-- | The Amazon S3 location to write output data.
labelingJobOutputConfig_s3OutputPath :: Lens.Lens' LabelingJobOutputConfig Prelude.Text
labelingJobOutputConfig_s3OutputPath = Lens.lens (\LabelingJobOutputConfig' {s3OutputPath} -> s3OutputPath) (\s@LabelingJobOutputConfig' {} a -> s {s3OutputPath = a} :: LabelingJobOutputConfig)

instance Data.FromJSON LabelingJobOutputConfig where
  parseJSON =
    Data.withObject
      "LabelingJobOutputConfig"
      ( \x ->
          LabelingJobOutputConfig'
            Prelude.<$> (x Data..:? "KmsKeyId")
            Prelude.<*> (x Data..:? "SnsTopicArn")
            Prelude.<*> (x Data..: "S3OutputPath")
      )

instance Prelude.Hashable LabelingJobOutputConfig where
  hashWithSalt _salt LabelingJobOutputConfig' {..} =
    _salt
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` snsTopicArn
      `Prelude.hashWithSalt` s3OutputPath

instance Prelude.NFData LabelingJobOutputConfig where
  rnf LabelingJobOutputConfig' {..} =
    Prelude.rnf kmsKeyId `Prelude.seq`
      Prelude.rnf snsTopicArn `Prelude.seq`
        Prelude.rnf s3OutputPath

instance Data.ToJSON LabelingJobOutputConfig where
  toJSON LabelingJobOutputConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("SnsTopicArn" Data..=) Prelude.<$> snsTopicArn,
            Prelude.Just ("S3OutputPath" Data..= s3OutputPath)
          ]
      )
