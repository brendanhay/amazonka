{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.AnalyticsS3BucketDestination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.AnalyticsS3BucketDestination where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.AnalyticsS3ExportFileFormat

-- | Contains information about where to publish the analytics results.
--
--
--
-- /See:/ 'analyticsS3BucketDestination' smart constructor.
data AnalyticsS3BucketDestination = AnalyticsS3BucketDestination'
  { _asbdBucketAccountId ::
      !(Maybe Text),
    _asbdPrefix :: !(Maybe Text),
    _asbdFormat ::
      !AnalyticsS3ExportFileFormat,
    _asbdBucket :: !BucketName
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AnalyticsS3BucketDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'asbdBucketAccountId' - The account ID that owns the destination S3 bucket. If no account ID is provided, the owner is not validated before exporting data.
--
-- * 'asbdPrefix' - The prefix to use when exporting data. The prefix is prepended to all results.
--
-- * 'asbdFormat' - Specifies the file format used when exporting data to Amazon S3.
--
-- * 'asbdBucket' - The Amazon Resource Name (ARN) of the bucket to which data is exported.
analyticsS3BucketDestination ::
  -- | 'asbdFormat'
  AnalyticsS3ExportFileFormat ->
  -- | 'asbdBucket'
  BucketName ->
  AnalyticsS3BucketDestination
analyticsS3BucketDestination pFormat_ pBucket_ =
  AnalyticsS3BucketDestination'
    { _asbdBucketAccountId = Nothing,
      _asbdPrefix = Nothing,
      _asbdFormat = pFormat_,
      _asbdBucket = pBucket_
    }

-- | The account ID that owns the destination S3 bucket. If no account ID is provided, the owner is not validated before exporting data.
asbdBucketAccountId :: Lens' AnalyticsS3BucketDestination (Maybe Text)
asbdBucketAccountId = lens _asbdBucketAccountId (\s a -> s {_asbdBucketAccountId = a})

-- | The prefix to use when exporting data. The prefix is prepended to all results.
asbdPrefix :: Lens' AnalyticsS3BucketDestination (Maybe Text)
asbdPrefix = lens _asbdPrefix (\s a -> s {_asbdPrefix = a})

-- | Specifies the file format used when exporting data to Amazon S3.
asbdFormat :: Lens' AnalyticsS3BucketDestination AnalyticsS3ExportFileFormat
asbdFormat = lens _asbdFormat (\s a -> s {_asbdFormat = a})

-- | The Amazon Resource Name (ARN) of the bucket to which data is exported.
asbdBucket :: Lens' AnalyticsS3BucketDestination BucketName
asbdBucket = lens _asbdBucket (\s a -> s {_asbdBucket = a})

instance FromXML AnalyticsS3BucketDestination where
  parseXML x =
    AnalyticsS3BucketDestination'
      <$> (x .@? "BucketAccountId")
      <*> (x .@? "Prefix")
      <*> (x .@ "Format")
      <*> (x .@ "Bucket")

instance Hashable AnalyticsS3BucketDestination

instance NFData AnalyticsS3BucketDestination

instance ToXML AnalyticsS3BucketDestination where
  toXML AnalyticsS3BucketDestination' {..} =
    mconcat
      [ "BucketAccountId" @= _asbdBucketAccountId,
        "Prefix" @= _asbdPrefix,
        "Format" @= _asbdFormat,
        "Bucket" @= _asbdBucket
      ]
