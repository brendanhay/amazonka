{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.LabelingJobOutputConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.LabelingJobOutputConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Output configuration information for a labeling job.
--
--
--
-- /See:/ 'labelingJobOutputConfig' smart constructor.
data LabelingJobOutputConfig = LabelingJobOutputConfig'
  { _ljocSNSTopicARN ::
      !(Maybe Text),
    _ljocKMSKeyId :: !(Maybe Text),
    _ljocS3OutputPath :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LabelingJobOutputConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ljocSNSTopicARN' - An Amazon Simple Notification Service (Amazon SNS) output topic ARN. When workers complete labeling tasks, Ground Truth will send labeling task output data to the SNS output topic you specify here. You must provide a value for this parameter if you provide an Amazon SNS input topic in @SnsDataSource@ in @InputConfig@ .
--
-- * 'ljocKMSKeyId' - The AWS Key Management Service ID of the key used to encrypt the output data, if any. If you use a KMS key ID or an alias of your master key, the Amazon SageMaker execution role must include permissions to call @kms:Encrypt@ . If you don't provide a KMS key ID, Amazon SageMaker uses the default KMS key for Amazon S3 for your role's account. Amazon SageMaker uses server-side encryption with KMS-managed keys for @LabelingJobOutputConfig@ . If you use a bucket policy with an @s3:PutObject@ permission that only allows objects with server-side encryption, set the condition key of @s3:x-amz-server-side-encryption@ to @"aws:kms"@ . For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html KMS-Managed Encryption Keys> in the /Amazon Simple Storage Service Developer Guide./  The KMS key policy must grant permission to the IAM role that you specify in your @CreateLabelingJob@ request. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Using Key Policies in AWS KMS> in the /AWS Key Management Service Developer Guide/ .
--
-- * 'ljocS3OutputPath' - The Amazon S3 location to write output data.
labelingJobOutputConfig ::
  -- | 'ljocS3OutputPath'
  Text ->
  LabelingJobOutputConfig
labelingJobOutputConfig pS3OutputPath_ =
  LabelingJobOutputConfig'
    { _ljocSNSTopicARN = Nothing,
      _ljocKMSKeyId = Nothing,
      _ljocS3OutputPath = pS3OutputPath_
    }

-- | An Amazon Simple Notification Service (Amazon SNS) output topic ARN. When workers complete labeling tasks, Ground Truth will send labeling task output data to the SNS output topic you specify here. You must provide a value for this parameter if you provide an Amazon SNS input topic in @SnsDataSource@ in @InputConfig@ .
ljocSNSTopicARN :: Lens' LabelingJobOutputConfig (Maybe Text)
ljocSNSTopicARN = lens _ljocSNSTopicARN (\s a -> s {_ljocSNSTopicARN = a})

-- | The AWS Key Management Service ID of the key used to encrypt the output data, if any. If you use a KMS key ID or an alias of your master key, the Amazon SageMaker execution role must include permissions to call @kms:Encrypt@ . If you don't provide a KMS key ID, Amazon SageMaker uses the default KMS key for Amazon S3 for your role's account. Amazon SageMaker uses server-side encryption with KMS-managed keys for @LabelingJobOutputConfig@ . If you use a bucket policy with an @s3:PutObject@ permission that only allows objects with server-side encryption, set the condition key of @s3:x-amz-server-side-encryption@ to @"aws:kms"@ . For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html KMS-Managed Encryption Keys> in the /Amazon Simple Storage Service Developer Guide./  The KMS key policy must grant permission to the IAM role that you specify in your @CreateLabelingJob@ request. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Using Key Policies in AWS KMS> in the /AWS Key Management Service Developer Guide/ .
ljocKMSKeyId :: Lens' LabelingJobOutputConfig (Maybe Text)
ljocKMSKeyId = lens _ljocKMSKeyId (\s a -> s {_ljocKMSKeyId = a})

-- | The Amazon S3 location to write output data.
ljocS3OutputPath :: Lens' LabelingJobOutputConfig Text
ljocS3OutputPath = lens _ljocS3OutputPath (\s a -> s {_ljocS3OutputPath = a})

instance FromJSON LabelingJobOutputConfig where
  parseJSON =
    withObject
      "LabelingJobOutputConfig"
      ( \x ->
          LabelingJobOutputConfig'
            <$> (x .:? "SnsTopicArn")
            <*> (x .:? "KmsKeyId")
            <*> (x .: "S3OutputPath")
      )

instance Hashable LabelingJobOutputConfig

instance NFData LabelingJobOutputConfig

instance ToJSON LabelingJobOutputConfig where
  toJSON LabelingJobOutputConfig' {..} =
    object
      ( catMaybes
          [ ("SnsTopicArn" .=) <$> _ljocSNSTopicARN,
            ("KmsKeyId" .=) <$> _ljocKMSKeyId,
            Just ("S3OutputPath" .= _ljocS3OutputPath)
          ]
      )
