{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.S3Configuration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.S3Configuration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides a description of an Amazon S3 data source, including the Amazon Resource Name (ARN) of the S3 bucket, the ARN of the IAM role that is used to access the bucket, and the name of the Amazon S3 object that contains the data.
--
--
--
-- /See:/ 's3Configuration' smart constructor.
data S3Configuration = S3Configuration'
  { _scRoleARN :: !Text,
    _scBucketARN :: !Text,
    _scFileKey :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'S3Configuration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scRoleARN' - IAM ARN of the role used to access the data.
--
-- * 'scBucketARN' - ARN of the S3 bucket that contains the data.
--
-- * 'scFileKey' - The name of the object that contains the data.
s3Configuration ::
  -- | 'scRoleARN'
  Text ->
  -- | 'scBucketARN'
  Text ->
  -- | 'scFileKey'
  Text ->
  S3Configuration
s3Configuration pRoleARN_ pBucketARN_ pFileKey_ =
  S3Configuration'
    { _scRoleARN = pRoleARN_,
      _scBucketARN = pBucketARN_,
      _scFileKey = pFileKey_
    }

-- | IAM ARN of the role used to access the data.
scRoleARN :: Lens' S3Configuration Text
scRoleARN = lens _scRoleARN (\s a -> s {_scRoleARN = a})

-- | ARN of the S3 bucket that contains the data.
scBucketARN :: Lens' S3Configuration Text
scBucketARN = lens _scBucketARN (\s a -> s {_scBucketARN = a})

-- | The name of the object that contains the data.
scFileKey :: Lens' S3Configuration Text
scFileKey = lens _scFileKey (\s a -> s {_scFileKey = a})

instance Hashable S3Configuration

instance NFData S3Configuration

instance ToJSON S3Configuration where
  toJSON S3Configuration' {..} =
    object
      ( catMaybes
          [ Just ("RoleARN" .= _scRoleARN),
            Just ("BucketARN" .= _scBucketARN),
            Just ("FileKey" .= _scFileKey)
          ]
      )
