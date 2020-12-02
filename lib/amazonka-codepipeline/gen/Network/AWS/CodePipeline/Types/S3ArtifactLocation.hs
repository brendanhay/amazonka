{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.S3ArtifactLocation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.S3ArtifactLocation where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The location of the S3 bucket that contains a revision.
--
--
--
-- /See:/ 's3ArtifactLocation' smart constructor.
data S3ArtifactLocation = S3ArtifactLocation'
  { _salBucketName ::
      !Text,
    _salObjectKey :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'S3ArtifactLocation' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'salBucketName' - The name of the S3 bucket.
--
-- * 'salObjectKey' - The key of the object in the S3 bucket, which uniquely identifies the object in the bucket.
s3ArtifactLocation ::
  -- | 'salBucketName'
  Text ->
  -- | 'salObjectKey'
  Text ->
  S3ArtifactLocation
s3ArtifactLocation pBucketName_ pObjectKey_ =
  S3ArtifactLocation'
    { _salBucketName = pBucketName_,
      _salObjectKey = pObjectKey_
    }

-- | The name of the S3 bucket.
salBucketName :: Lens' S3ArtifactLocation Text
salBucketName = lens _salBucketName (\s a -> s {_salBucketName = a})

-- | The key of the object in the S3 bucket, which uniquely identifies the object in the bucket.
salObjectKey :: Lens' S3ArtifactLocation Text
salObjectKey = lens _salObjectKey (\s a -> s {_salObjectKey = a})

instance FromJSON S3ArtifactLocation where
  parseJSON =
    withObject
      "S3ArtifactLocation"
      ( \x ->
          S3ArtifactLocation' <$> (x .: "bucketName") <*> (x .: "objectKey")
      )

instance Hashable S3ArtifactLocation

instance NFData S3ArtifactLocation
