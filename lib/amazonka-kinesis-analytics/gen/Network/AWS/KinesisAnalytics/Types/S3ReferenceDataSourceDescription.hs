{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSourceDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSourceDescription where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides the bucket name and object key name that stores the reference data.
--
--
--
-- /See:/ 's3ReferenceDataSourceDescription' smart constructor.
data S3ReferenceDataSourceDescription = S3ReferenceDataSourceDescription'
  { _srdsdBucketARN ::
      !Text,
    _srdsdFileKey :: !Text,
    _srdsdReferenceRoleARN ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'S3ReferenceDataSourceDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srdsdBucketARN' - Amazon Resource Name (ARN) of the S3 bucket.
--
-- * 'srdsdFileKey' - Amazon S3 object key name.
--
-- * 'srdsdReferenceRoleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to read the Amazon S3 object on your behalf to populate the in-application reference table.
s3ReferenceDataSourceDescription ::
  -- | 'srdsdBucketARN'
  Text ->
  -- | 'srdsdFileKey'
  Text ->
  -- | 'srdsdReferenceRoleARN'
  Text ->
  S3ReferenceDataSourceDescription
s3ReferenceDataSourceDescription
  pBucketARN_
  pFileKey_
  pReferenceRoleARN_ =
    S3ReferenceDataSourceDescription'
      { _srdsdBucketARN = pBucketARN_,
        _srdsdFileKey = pFileKey_,
        _srdsdReferenceRoleARN = pReferenceRoleARN_
      }

-- | Amazon Resource Name (ARN) of the S3 bucket.
srdsdBucketARN :: Lens' S3ReferenceDataSourceDescription Text
srdsdBucketARN = lens _srdsdBucketARN (\s a -> s {_srdsdBucketARN = a})

-- | Amazon S3 object key name.
srdsdFileKey :: Lens' S3ReferenceDataSourceDescription Text
srdsdFileKey = lens _srdsdFileKey (\s a -> s {_srdsdFileKey = a})

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to read the Amazon S3 object on your behalf to populate the in-application reference table.
srdsdReferenceRoleARN :: Lens' S3ReferenceDataSourceDescription Text
srdsdReferenceRoleARN = lens _srdsdReferenceRoleARN (\s a -> s {_srdsdReferenceRoleARN = a})

instance FromJSON S3ReferenceDataSourceDescription where
  parseJSON =
    withObject
      "S3ReferenceDataSourceDescription"
      ( \x ->
          S3ReferenceDataSourceDescription'
            <$> (x .: "BucketARN")
            <*> (x .: "FileKey")
            <*> (x .: "ReferenceRoleARN")
      )

instance Hashable S3ReferenceDataSourceDescription

instance NFData S3ReferenceDataSourceDescription
