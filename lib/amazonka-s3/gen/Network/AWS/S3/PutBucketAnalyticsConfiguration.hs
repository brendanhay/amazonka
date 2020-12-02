{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutBucketAnalyticsConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets an analytics configuration for the bucket (specified by the analytics configuration ID). You can have up to 1,000 analytics configurations per bucket.
--
--
-- You can choose to have storage class analysis export analysis reports sent to a comma-separated values (CSV) flat file. See the @DataExport@ request element. Reports are updated daily and are based on the object filters that you configure. When selecting data export, you specify a destination bucket and an optional destination prefix where the file is written. You can export the data to a destination bucket in a different account. However, the destination bucket must be in the same Region as the bucket that you are making the PUT analytics configuration to. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/analytics-storage-class.html Amazon S3 Analytics – Storage Class Analysis> .
--
-- /Important:/ You must create a bucket policy on the destination bucket where the exported file is written to grant permissions to Amazon S3 to write objects to the bucket. For an example policy, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/example-bucket-policies.html#example-bucket-policies-use-case-9 Granting Permissions for Amazon S3 Inventory and Storage Class Analysis> .
--
-- To use this operation, you must have permissions to perform the @s3:PutAnalyticsConfiguration@ action. The bucket owner has this permission by default. The bucket owner can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
--
-- __Special Errors__
--
--     *     * /HTTP Error: HTTP 400 Bad Request/
--
--     * /Code: InvalidArgument/
--
--     * /Cause: Invalid argument./
--
--
--
--     *     * /HTTP Error: HTTP 400 Bad Request/
--
--     * /Code: TooManyConfigurations/
--
--     * /Cause: You are attempting to create a new configuration but have already reached the 1,000-configuration limit./
--
--
--
--     *     * /HTTP Error: HTTP 403 Forbidden/
--
--     * /Code: AccessDenied/
--
--     * /Cause: You are not the owner of the specified bucket, or you do not have the s3:PutAnalyticsConfiguration bucket permission to set the configuration on the bucket./
--
--
--
--
--
-- __Related Resources__
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketAnalyticsConfiguration.html GetBucketAnalyticsConfiguration>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketAnalyticsConfiguration.html DeleteBucketAnalyticsConfiguration>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketAnalyticsConfigurations.html ListBucketAnalyticsConfigurations>
module Network.AWS.S3.PutBucketAnalyticsConfiguration
  ( -- * Creating a Request
    putBucketAnalyticsConfiguration,
    PutBucketAnalyticsConfiguration,

    -- * Request Lenses
    pExpectedBucketOwner,
    pBucket,
    pId,
    pAnalyticsConfiguration,

    -- * Destructuring the Response
    putBucketAnalyticsConfigurationResponse,
    PutBucketAnalyticsConfigurationResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'putBucketAnalyticsConfiguration' smart constructor.
data PutBucketAnalyticsConfiguration = PutBucketAnalyticsConfiguration'
  { _pExpectedBucketOwner ::
      !(Maybe Text),
    _pBucket :: !BucketName,
    _pId :: !Text,
    _pAnalyticsConfiguration ::
      !AnalyticsConfiguration
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutBucketAnalyticsConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'pBucket' - The name of the bucket to which an analytics configuration is stored.
--
-- * 'pId' - The ID that identifies the analytics configuration.
--
-- * 'pAnalyticsConfiguration' - The configuration and any analyses for the analytics filter.
putBucketAnalyticsConfiguration ::
  -- | 'pBucket'
  BucketName ->
  -- | 'pId'
  Text ->
  -- | 'pAnalyticsConfiguration'
  AnalyticsConfiguration ->
  PutBucketAnalyticsConfiguration
putBucketAnalyticsConfiguration
  pBucket_
  pId_
  pAnalyticsConfiguration_ =
    PutBucketAnalyticsConfiguration'
      { _pExpectedBucketOwner = Nothing,
        _pBucket = pBucket_,
        _pId = pId_,
        _pAnalyticsConfiguration = pAnalyticsConfiguration_
      }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
pExpectedBucketOwner :: Lens' PutBucketAnalyticsConfiguration (Maybe Text)
pExpectedBucketOwner = lens _pExpectedBucketOwner (\s a -> s {_pExpectedBucketOwner = a})

-- | The name of the bucket to which an analytics configuration is stored.
pBucket :: Lens' PutBucketAnalyticsConfiguration BucketName
pBucket = lens _pBucket (\s a -> s {_pBucket = a})

-- | The ID that identifies the analytics configuration.
pId :: Lens' PutBucketAnalyticsConfiguration Text
pId = lens _pId (\s a -> s {_pId = a})

-- | The configuration and any analyses for the analytics filter.
pAnalyticsConfiguration :: Lens' PutBucketAnalyticsConfiguration AnalyticsConfiguration
pAnalyticsConfiguration = lens _pAnalyticsConfiguration (\s a -> s {_pAnalyticsConfiguration = a})

instance AWSRequest PutBucketAnalyticsConfiguration where
  type
    Rs PutBucketAnalyticsConfiguration =
      PutBucketAnalyticsConfigurationResponse
  request = putXML s3
  response = receiveNull PutBucketAnalyticsConfigurationResponse'

instance Hashable PutBucketAnalyticsConfiguration

instance NFData PutBucketAnalyticsConfiguration

instance ToElement PutBucketAnalyticsConfiguration where
  toElement =
    mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}AnalyticsConfiguration"
      . _pAnalyticsConfiguration

instance ToHeaders PutBucketAnalyticsConfiguration where
  toHeaders PutBucketAnalyticsConfiguration' {..} =
    mconcat ["x-amz-expected-bucket-owner" =# _pExpectedBucketOwner]

instance ToPath PutBucketAnalyticsConfiguration where
  toPath PutBucketAnalyticsConfiguration' {..} =
    mconcat ["/", toBS _pBucket]

instance ToQuery PutBucketAnalyticsConfiguration where
  toQuery PutBucketAnalyticsConfiguration' {..} =
    mconcat ["id" =: _pId, "analytics"]

-- | /See:/ 'putBucketAnalyticsConfigurationResponse' smart constructor.
data PutBucketAnalyticsConfigurationResponse = PutBucketAnalyticsConfigurationResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutBucketAnalyticsConfigurationResponse' with the minimum fields required to make a request.
putBucketAnalyticsConfigurationResponse ::
  PutBucketAnalyticsConfigurationResponse
putBucketAnalyticsConfigurationResponse =
  PutBucketAnalyticsConfigurationResponse'

instance NFData PutBucketAnalyticsConfigurationResponse
