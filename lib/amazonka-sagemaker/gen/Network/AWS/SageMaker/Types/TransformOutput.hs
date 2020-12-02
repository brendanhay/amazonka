{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TransformOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TransformOutput where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.AssemblyType

-- | Describes the results of a transform job.
--
--
--
-- /See:/ 'transformOutput' smart constructor.
data TransformOutput = TransformOutput'
  { _toAssembleWith ::
      !(Maybe AssemblyType),
    _toAccept :: !(Maybe Text),
    _toKMSKeyId :: !(Maybe Text),
    _toS3OutputPath :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'TransformOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'toAssembleWith' - Defines how to assemble the results of the transform job as a single S3 object. Choose a format that is most convenient to you. To concatenate the results in binary format, specify @None@ . To add a newline character at the end of every transformed record, specify @Line@ .
--
-- * 'toAccept' - The MIME type used to specify the output data. Amazon SageMaker uses the MIME type with each http call to transfer data from the transform job.
--
-- * 'toKMSKeyId' - The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt the model artifacts at rest using Amazon S3 server-side encryption. The @KmsKeyId@ can be any of the following formats:      * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@      * Alias name: @alias/ExampleAlias@      * Alias name ARN: @arn:aws:kms:us-west-2:111122223333:alias/ExampleAlias@  If you don't provide a KMS key ID, Amazon SageMaker uses the default KMS key for Amazon S3 for your role's account. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html KMS-Managed Encryption Keys> in the /Amazon Simple Storage Service Developer Guide./  The KMS key policy must grant permission to the IAM role that you specify in your 'CreateModel' request. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Using Key Policies in AWS KMS> in the /AWS Key Management Service Developer Guide/ .
--
-- * 'toS3OutputPath' - The Amazon S3 path where you want Amazon SageMaker to store the results of the transform job. For example, @s3://bucket-name/key-name-prefix@ . For every S3 object used as input for the transform job, batch transform stores the transformed data with an .@out@ suffix in a corresponding subfolder in the location in the output prefix. For example, for the input data stored at @s3://bucket-name/input-name-prefix/dataset01/data.csv@ , batch transform stores the transformed data at @s3://bucket-name/output-name-prefix/input-name-prefix/data.csv.out@ . Batch transform doesn't upload partially processed objects. For an input S3 object that contains multiple records, it creates an .@out@ file only if the transform job succeeds on the entire file. When the input contains multiple S3 objects, the batch transform job processes the listed S3 objects and uploads only the output for successfully processed objects. If any object fails in the transform job batch transform marks the job as failed to prompt investigation.
transformOutput ::
  -- | 'toS3OutputPath'
  Text ->
  TransformOutput
transformOutput pS3OutputPath_ =
  TransformOutput'
    { _toAssembleWith = Nothing,
      _toAccept = Nothing,
      _toKMSKeyId = Nothing,
      _toS3OutputPath = pS3OutputPath_
    }

-- | Defines how to assemble the results of the transform job as a single S3 object. Choose a format that is most convenient to you. To concatenate the results in binary format, specify @None@ . To add a newline character at the end of every transformed record, specify @Line@ .
toAssembleWith :: Lens' TransformOutput (Maybe AssemblyType)
toAssembleWith = lens _toAssembleWith (\s a -> s {_toAssembleWith = a})

-- | The MIME type used to specify the output data. Amazon SageMaker uses the MIME type with each http call to transfer data from the transform job.
toAccept :: Lens' TransformOutput (Maybe Text)
toAccept = lens _toAccept (\s a -> s {_toAccept = a})

-- | The AWS Key Management Service (AWS KMS) key that Amazon SageMaker uses to encrypt the model artifacts at rest using Amazon S3 server-side encryption. The @KmsKeyId@ can be any of the following formats:      * Key ID: @1234abcd-12ab-34cd-56ef-1234567890ab@      * Key ARN: @arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab@      * Alias name: @alias/ExampleAlias@      * Alias name ARN: @arn:aws:kms:us-west-2:111122223333:alias/ExampleAlias@  If you don't provide a KMS key ID, Amazon SageMaker uses the default KMS key for Amazon S3 for your role's account. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/UsingKMSEncryption.html KMS-Managed Encryption Keys> in the /Amazon Simple Storage Service Developer Guide./  The KMS key policy must grant permission to the IAM role that you specify in your 'CreateModel' request. For more information, see <http://docs.aws.amazon.com/kms/latest/developerguide/key-policies.html Using Key Policies in AWS KMS> in the /AWS Key Management Service Developer Guide/ .
toKMSKeyId :: Lens' TransformOutput (Maybe Text)
toKMSKeyId = lens _toKMSKeyId (\s a -> s {_toKMSKeyId = a})

-- | The Amazon S3 path where you want Amazon SageMaker to store the results of the transform job. For example, @s3://bucket-name/key-name-prefix@ . For every S3 object used as input for the transform job, batch transform stores the transformed data with an .@out@ suffix in a corresponding subfolder in the location in the output prefix. For example, for the input data stored at @s3://bucket-name/input-name-prefix/dataset01/data.csv@ , batch transform stores the transformed data at @s3://bucket-name/output-name-prefix/input-name-prefix/data.csv.out@ . Batch transform doesn't upload partially processed objects. For an input S3 object that contains multiple records, it creates an .@out@ file only if the transform job succeeds on the entire file. When the input contains multiple S3 objects, the batch transform job processes the listed S3 objects and uploads only the output for successfully processed objects. If any object fails in the transform job batch transform marks the job as failed to prompt investigation.
toS3OutputPath :: Lens' TransformOutput Text
toS3OutputPath = lens _toS3OutputPath (\s a -> s {_toS3OutputPath = a})

instance FromJSON TransformOutput where
  parseJSON =
    withObject
      "TransformOutput"
      ( \x ->
          TransformOutput'
            <$> (x .:? "AssembleWith")
            <*> (x .:? "Accept")
            <*> (x .:? "KmsKeyId")
            <*> (x .: "S3OutputPath")
      )

instance Hashable TransformOutput

instance NFData TransformOutput

instance ToJSON TransformOutput where
  toJSON TransformOutput' {..} =
    object
      ( catMaybes
          [ ("AssembleWith" .=) <$> _toAssembleWith,
            ("Accept" .=) <$> _toAccept,
            ("KmsKeyId" .=) <$> _toKMSKeyId,
            Just ("S3OutputPath" .= _toS3OutputPath)
          ]
      )
