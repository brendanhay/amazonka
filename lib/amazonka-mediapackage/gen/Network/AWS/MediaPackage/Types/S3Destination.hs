{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaPackage.Types.S3Destination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaPackage.Types.S3Destination where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Configuration parameters for where in an S3 bucket to place the harvested content
--
-- /See:/ 's3Destination' smart constructor.
data S3Destination = S3Destination'
  { _sdManifestKey :: !Text,
    _sdBucketName :: !Text,
    _sdRoleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'S3Destination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdManifestKey' - The key in the specified S3 bucket where the harvested top-level manifest will be placed.
--
-- * 'sdBucketName' - The name of an S3 bucket within which harvested content will be exported
--
-- * 'sdRoleARN' - The IAM role used to write to the specified S3 bucket
s3Destination ::
  -- | 'sdManifestKey'
  Text ->
  -- | 'sdBucketName'
  Text ->
  -- | 'sdRoleARN'
  Text ->
  S3Destination
s3Destination pManifestKey_ pBucketName_ pRoleARN_ =
  S3Destination'
    { _sdManifestKey = pManifestKey_,
      _sdBucketName = pBucketName_,
      _sdRoleARN = pRoleARN_
    }

-- | The key in the specified S3 bucket where the harvested top-level manifest will be placed.
sdManifestKey :: Lens' S3Destination Text
sdManifestKey = lens _sdManifestKey (\s a -> s {_sdManifestKey = a})

-- | The name of an S3 bucket within which harvested content will be exported
sdBucketName :: Lens' S3Destination Text
sdBucketName = lens _sdBucketName (\s a -> s {_sdBucketName = a})

-- | The IAM role used to write to the specified S3 bucket
sdRoleARN :: Lens' S3Destination Text
sdRoleARN = lens _sdRoleARN (\s a -> s {_sdRoleARN = a})

instance FromJSON S3Destination where
  parseJSON =
    withObject
      "S3Destination"
      ( \x ->
          S3Destination'
            <$> (x .: "manifestKey") <*> (x .: "bucketName") <*> (x .: "roleArn")
      )

instance Hashable S3Destination

instance NFData S3Destination

instance ToJSON S3Destination where
  toJSON S3Destination' {..} =
    object
      ( catMaybes
          [ Just ("manifestKey" .= _sdManifestKey),
            Just ("bucketName" .= _sdBucketName),
            Just ("roleArn" .= _sdRoleARN)
          ]
      )
