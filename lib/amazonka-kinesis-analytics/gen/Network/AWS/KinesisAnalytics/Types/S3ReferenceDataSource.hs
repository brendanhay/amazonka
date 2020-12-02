{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.S3ReferenceDataSource where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Identifies the S3 bucket and object that contains the reference data. Also identifies the IAM role Amazon Kinesis Analytics can assume to read this object on your behalf.
--
--
-- An Amazon Kinesis Analytics application loads reference data only once. If the data changes, you call the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_UpdateApplication.html UpdateApplication> operation to trigger reloading of data into your application.
--
--
-- /See:/ 's3ReferenceDataSource' smart constructor.
data S3ReferenceDataSource = S3ReferenceDataSource'
  { _srdsBucketARN ::
      !Text,
    _srdsFileKey :: !Text,
    _srdsReferenceRoleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'S3ReferenceDataSource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srdsBucketARN' - Amazon Resource Name (ARN) of the S3 bucket.
--
-- * 'srdsFileKey' - Object key name containing reference data.
--
-- * 'srdsReferenceRoleARN' - ARN of the IAM role that the service can assume to read data on your behalf. This role must have permission for the @s3:GetObject@ action on the object and trust policy that allows Amazon Kinesis Analytics service principal to assume this role.
s3ReferenceDataSource ::
  -- | 'srdsBucketARN'
  Text ->
  -- | 'srdsFileKey'
  Text ->
  -- | 'srdsReferenceRoleARN'
  Text ->
  S3ReferenceDataSource
s3ReferenceDataSource pBucketARN_ pFileKey_ pReferenceRoleARN_ =
  S3ReferenceDataSource'
    { _srdsBucketARN = pBucketARN_,
      _srdsFileKey = pFileKey_,
      _srdsReferenceRoleARN = pReferenceRoleARN_
    }

-- | Amazon Resource Name (ARN) of the S3 bucket.
srdsBucketARN :: Lens' S3ReferenceDataSource Text
srdsBucketARN = lens _srdsBucketARN (\s a -> s {_srdsBucketARN = a})

-- | Object key name containing reference data.
srdsFileKey :: Lens' S3ReferenceDataSource Text
srdsFileKey = lens _srdsFileKey (\s a -> s {_srdsFileKey = a})

-- | ARN of the IAM role that the service can assume to read data on your behalf. This role must have permission for the @s3:GetObject@ action on the object and trust policy that allows Amazon Kinesis Analytics service principal to assume this role.
srdsReferenceRoleARN :: Lens' S3ReferenceDataSource Text
srdsReferenceRoleARN = lens _srdsReferenceRoleARN (\s a -> s {_srdsReferenceRoleARN = a})

instance Hashable S3ReferenceDataSource

instance NFData S3ReferenceDataSource

instance ToJSON S3ReferenceDataSource where
  toJSON S3ReferenceDataSource' {..} =
    object
      ( catMaybes
          [ Just ("BucketARN" .= _srdsBucketARN),
            Just ("FileKey" .= _srdsFileKey),
            Just ("ReferenceRoleARN" .= _srdsReferenceRoleARN)
          ]
      )
