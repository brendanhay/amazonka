{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.OutputDataConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.OutputDataConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides configuration parameters for the output of topic detection jobs.
--
--
--
--
--
-- /See:/ 'outputDataConfig' smart constructor.
data OutputDataConfig = OutputDataConfig'
  { _odcKMSKeyId ::
      !(Maybe Text),
    _odcS3URI :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'OutputDataConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'odcKMSKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt the output results from an analysis job. The KmsKeyId can be one of the following formats:     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@      * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@      * KMS Key Alias: @"alias/ExampleAlias"@      * ARN of a KMS Key Alias: @"arn:aws:kms:us-west-2:111122223333:alias/ExampleAlias"@
--
-- * 'odcS3URI' - When you use the @OutputDataConfig@ object with asynchronous operations, you specify the Amazon S3 location where you want to write the output data. The URI must be in the same region as the API endpoint that you are calling. The location is used as the prefix for the actual location of the output file. When the topic detection job is finished, the service creates an output file in a directory specific to the job. The @S3Uri@ field contains the location of the output file, called @output.tar.gz@ . It is a compressed archive that contains the ouput of the operation.
outputDataConfig ::
  -- | 'odcS3URI'
  Text ->
  OutputDataConfig
outputDataConfig pS3URI_ =
  OutputDataConfig' {_odcKMSKeyId = Nothing, _odcS3URI = pS3URI_}

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt the output results from an analysis job. The KmsKeyId can be one of the following formats:     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@      * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@      * KMS Key Alias: @"alias/ExampleAlias"@      * ARN of a KMS Key Alias: @"arn:aws:kms:us-west-2:111122223333:alias/ExampleAlias"@
odcKMSKeyId :: Lens' OutputDataConfig (Maybe Text)
odcKMSKeyId = lens _odcKMSKeyId (\s a -> s {_odcKMSKeyId = a})

-- | When you use the @OutputDataConfig@ object with asynchronous operations, you specify the Amazon S3 location where you want to write the output data. The URI must be in the same region as the API endpoint that you are calling. The location is used as the prefix for the actual location of the output file. When the topic detection job is finished, the service creates an output file in a directory specific to the job. The @S3Uri@ field contains the location of the output file, called @output.tar.gz@ . It is a compressed archive that contains the ouput of the operation.
odcS3URI :: Lens' OutputDataConfig Text
odcS3URI = lens _odcS3URI (\s a -> s {_odcS3URI = a})

instance FromJSON OutputDataConfig where
  parseJSON =
    withObject
      "OutputDataConfig"
      ( \x ->
          OutputDataConfig' <$> (x .:? "KmsKeyId") <*> (x .: "S3Uri")
      )

instance Hashable OutputDataConfig

instance NFData OutputDataConfig

instance ToJSON OutputDataConfig where
  toJSON OutputDataConfig' {..} =
    object
      ( catMaybes
          [("KmsKeyId" .=) <$> _odcKMSKeyId, Just ("S3Uri" .= _odcS3URI)]
      )
