{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.S3Location
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.S3Location where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The location in S3 where build or script files are stored for access by Amazon GameLift. This location is specified in 'CreateBuild' , 'CreateScript' , and 'UpdateScript' requests.
--
--
--
-- /See:/ 's3Location' smart constructor.
data S3Location = S3Location'
  { _slBucket :: !(Maybe Text),
    _slKey :: !(Maybe Text),
    _slObjectVersion :: !(Maybe Text),
    _slRoleARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'S3Location' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slBucket' - An S3 bucket identifier. This is the name of the S3 bucket.
--
-- * 'slKey' - The name of the zip file that contains the build files or script files.
--
-- * 'slObjectVersion' - The version of the file, if object versioning is turned on for the bucket. Amazon GameLift uses this information when retrieving files from an S3 bucket that you own. Use this parameter to specify a specific version of the file. If not set, the latest version of the file is retrieved.
--
-- * 'slRoleARN' - The Amazon Resource Name (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN> ) for an IAM role that allows Amazon GameLift to access the S3 bucket.
s3Location ::
  S3Location
s3Location =
  S3Location'
    { _slBucket = Nothing,
      _slKey = Nothing,
      _slObjectVersion = Nothing,
      _slRoleARN = Nothing
    }

-- | An S3 bucket identifier. This is the name of the S3 bucket.
slBucket :: Lens' S3Location (Maybe Text)
slBucket = lens _slBucket (\s a -> s {_slBucket = a})

-- | The name of the zip file that contains the build files or script files.
slKey :: Lens' S3Location (Maybe Text)
slKey = lens _slKey (\s a -> s {_slKey = a})

-- | The version of the file, if object versioning is turned on for the bucket. Amazon GameLift uses this information when retrieving files from an S3 bucket that you own. Use this parameter to specify a specific version of the file. If not set, the latest version of the file is retrieved.
slObjectVersion :: Lens' S3Location (Maybe Text)
slObjectVersion = lens _slObjectVersion (\s a -> s {_slObjectVersion = a})

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN> ) for an IAM role that allows Amazon GameLift to access the S3 bucket.
slRoleARN :: Lens' S3Location (Maybe Text)
slRoleARN = lens _slRoleARN (\s a -> s {_slRoleARN = a})

instance FromJSON S3Location where
  parseJSON =
    withObject
      "S3Location"
      ( \x ->
          S3Location'
            <$> (x .:? "Bucket")
            <*> (x .:? "Key")
            <*> (x .:? "ObjectVersion")
            <*> (x .:? "RoleArn")
      )

instance Hashable S3Location

instance NFData S3Location

instance ToJSON S3Location where
  toJSON S3Location' {..} =
    object
      ( catMaybes
          [ ("Bucket" .=) <$> _slBucket,
            ("Key" .=) <$> _slKey,
            ("ObjectVersion" .=) <$> _slObjectVersion,
            ("RoleArn" .=) <$> _slRoleARN
          ]
      )
