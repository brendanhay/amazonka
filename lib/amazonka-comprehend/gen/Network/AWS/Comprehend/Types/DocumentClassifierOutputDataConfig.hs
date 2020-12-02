{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.DocumentClassifierOutputDataConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.DocumentClassifierOutputDataConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides output results configuration parameters for custom classifier jobs.
--
--
--
-- /See:/ 'documentClassifierOutputDataConfig' smart constructor.
data DocumentClassifierOutputDataConfig = DocumentClassifierOutputDataConfig'
  { _dcodcKMSKeyId ::
      !(Maybe Text),
    _dcodcS3URI ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DocumentClassifierOutputDataConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcodcKMSKeyId' - ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt the output results from an analysis job. The KmsKeyId can be one of the following formats:     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@      * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@      * KMS Key Alias: @"alias/ExampleAlias"@      * ARN of a KMS Key Alias: @"arn:aws:kms:us-west-2:111122223333:alias/ExampleAlias"@
--
-- * 'dcodcS3URI' - When you use the @OutputDataConfig@ object while creating a custom classifier, you specify the Amazon S3 location where you want to write the confusion matrix. The URI must be in the same region as the API endpoint that you are calling. The location is used as the prefix for the actual location of this output file. When the custom classifier job is finished, the service creates the output file in a directory specific to the job. The @S3Uri@ field contains the location of the output file, called @output.tar.gz@ . It is a compressed archive that contains the confusion matrix.
documentClassifierOutputDataConfig ::
  DocumentClassifierOutputDataConfig
documentClassifierOutputDataConfig =
  DocumentClassifierOutputDataConfig'
    { _dcodcKMSKeyId = Nothing,
      _dcodcS3URI = Nothing
    }

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt the output results from an analysis job. The KmsKeyId can be one of the following formats:     * KMS Key ID: @"1234abcd-12ab-34cd-56ef-1234567890ab"@      * Amazon Resource Name (ARN) of a KMS Key: @"arn:aws:kms:us-west-2:111122223333:key/1234abcd-12ab-34cd-56ef-1234567890ab"@      * KMS Key Alias: @"alias/ExampleAlias"@      * ARN of a KMS Key Alias: @"arn:aws:kms:us-west-2:111122223333:alias/ExampleAlias"@
dcodcKMSKeyId :: Lens' DocumentClassifierOutputDataConfig (Maybe Text)
dcodcKMSKeyId = lens _dcodcKMSKeyId (\s a -> s {_dcodcKMSKeyId = a})

-- | When you use the @OutputDataConfig@ object while creating a custom classifier, you specify the Amazon S3 location where you want to write the confusion matrix. The URI must be in the same region as the API endpoint that you are calling. The location is used as the prefix for the actual location of this output file. When the custom classifier job is finished, the service creates the output file in a directory specific to the job. The @S3Uri@ field contains the location of the output file, called @output.tar.gz@ . It is a compressed archive that contains the confusion matrix.
dcodcS3URI :: Lens' DocumentClassifierOutputDataConfig (Maybe Text)
dcodcS3URI = lens _dcodcS3URI (\s a -> s {_dcodcS3URI = a})

instance FromJSON DocumentClassifierOutputDataConfig where
  parseJSON =
    withObject
      "DocumentClassifierOutputDataConfig"
      ( \x ->
          DocumentClassifierOutputDataConfig'
            <$> (x .:? "KmsKeyId") <*> (x .:? "S3Uri")
      )

instance Hashable DocumentClassifierOutputDataConfig

instance NFData DocumentClassifierOutputDataConfig

instance ToJSON DocumentClassifierOutputDataConfig where
  toJSON DocumentClassifierOutputDataConfig' {..} =
    object
      ( catMaybes
          [("KmsKeyId" .=) <$> _dcodcKMSKeyId, ("S3Uri" .=) <$> _dcodcS3URI]
      )
