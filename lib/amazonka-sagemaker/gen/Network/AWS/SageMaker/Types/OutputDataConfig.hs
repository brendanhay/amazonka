{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.OutputDataConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.OutputDataConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information about how to store model training results (model artifacts).
--
--
--
-- /See:/ 'outputDataConfig' smart constructor.
data OutputDataConfig = OutputDataConfig'
  { _odcKMSKeyId ::
      !(Maybe Text),
    _odcS3OutputPath :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OutputDataConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'odcKMSKeyId' - The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt the model artifacts at rest using Amazon S3 server-side encryption. The @KmsKeyId@ can be any of the following formats:      * // KMS Key ID @"1234abcd-12ab-34cd-56ef-1234567890ab"@      * // Amazon Resource Name (ARN) of a KMS Key @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@      * // KMS Key Alias @"alias/ExampleAlias"@      * // Amazon Resource Name (ARN) of a KMS Key Alias @"arn:aws:kms:us-west-2:111122223333:alias/ExampleAlias"@  If you use a KMS key ID or an alias of your master key, the Amazon SageMaker execution role must include permissions to call @kms:Encrypt@ . If you don't provide a KMS key ID, Amazon SageMaker uses the default KMS key for Amazon S3 for your role's account. Amazon SageMaker uses server-side encryption with KMS-managed keys for @OutputDataConfig@ . If you use a bucket policy with an @s3:PutObject@ permission that only allows objects with server-side encryption, set the condition key of @s3:x-amz-server-side-encryption@ to @"aws:kms"@ . For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html KMS-Managed Encryption Keys> in the /Amazon Simple Storage Service Developer Guide./  The KMS key policy must grant permission to the IAM role that you specify in your @CreateTrainingJob@ , @CreateTransformJob@ , or @CreateHyperParameterTuningJob@ requests. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Using Key Policies in AWS KMS> in the /AWS Key Management Service Developer Guide/ .
--
-- * 'odcS3OutputPath' - Identifies the S3 path where you want Amazon SageMaker to store the model artifacts. For example, @s3://bucket-name/key-name-prefix@ .
outputDataConfig ::
  -- | 'odcS3OutputPath'
  Text ->
  OutputDataConfig
outputDataConfig pS3OutputPath_ =
  OutputDataConfig'
    { _odcKMSKeyId = Nothing,
      _odcS3OutputPath = pS3OutputPath_
    }

-- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt the model artifacts at rest using Amazon S3 server-side encryption. The @KmsKeyId@ can be any of the following formats:      * // KMS Key ID @"1234abcd-12ab-34cd-56ef-1234567890ab"@      * // Amazon Resource Name (ARN) of a KMS Key @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@      * // KMS Key Alias @"alias/ExampleAlias"@      * // Amazon Resource Name (ARN) of a KMS Key Alias @"arn:aws:kms:us-west-2:111122223333:alias/ExampleAlias"@  If you use a KMS key ID or an alias of your master key, the Amazon SageMaker execution role must include permissions to call @kms:Encrypt@ . If you don't provide a KMS key ID, Amazon SageMaker uses the default KMS key for Amazon S3 for your role's account. Amazon SageMaker uses server-side encryption with KMS-managed keys for @OutputDataConfig@ . If you use a bucket policy with an @s3:PutObject@ permission that only allows objects with server-side encryption, set the condition key of @s3:x-amz-server-side-encryption@ to @"aws:kms"@ . For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html KMS-Managed Encryption Keys> in the /Amazon Simple Storage Service Developer Guide./  The KMS key policy must grant permission to the IAM role that you specify in your @CreateTrainingJob@ , @CreateTransformJob@ , or @CreateHyperParameterTuningJob@ requests. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Using Key Policies in AWS KMS> in the /AWS Key Management Service Developer Guide/ .
odcKMSKeyId :: Lens' OutputDataConfig (Maybe Text)
odcKMSKeyId = lens _odcKMSKeyId (\s a -> s {_odcKMSKeyId = a})

-- | Identifies the S3 path where you want Amazon SageMaker to store the model artifacts. For example, @s3://bucket-name/key-name-prefix@ .
odcS3OutputPath :: Lens' OutputDataConfig Text
odcS3OutputPath = lens _odcS3OutputPath (\s a -> s {_odcS3OutputPath = a})

instance FromJSON OutputDataConfig where
  parseJSON =
    withObject
      "OutputDataConfig"
      ( \x ->
          OutputDataConfig' <$> (x .:? "KmsKeyId") <*> (x .: "S3OutputPath")
      )

instance Hashable OutputDataConfig

instance NFData OutputDataConfig

instance ToJSON OutputDataConfig where
  toJSON OutputDataConfig' {..} =
    object
      ( catMaybes
          [ ("KmsKeyId" .=) <$> _odcKMSKeyId,
            Just ("S3OutputPath" .= _odcS3OutputPath)
          ]
      )
