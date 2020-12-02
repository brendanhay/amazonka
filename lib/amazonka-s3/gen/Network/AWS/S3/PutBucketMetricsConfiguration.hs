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
-- Module      : Network.AWS.S3.PutBucketMetricsConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets a metrics configuration (specified by the metrics configuration ID) for the bucket. You can have up to 1,000 metrics configurations per bucket. If you're updating an existing metrics configuration, note that this is a full replacement of the existing metrics configuration. If you don't include the elements you want to keep, they are erased.
--
--
-- To use this operation, you must have permissions to perform the @s3:PutMetricsConfiguration@ action. The bucket owner has this permission by default. The bucket owner can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
--
-- For information about CloudWatch request metrics for Amazon S3, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/cloudwatch-monitoring.html Monitoring Metrics with Amazon CloudWatch> .
--
-- The following operations are related to @PutBucketMetricsConfiguration@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketMetricsConfiguration.html DeleteBucketMetricsConfiguration>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketMetricsConfiguration.html PutBucketMetricsConfiguration>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketMetricsConfigurations.html ListBucketMetricsConfigurations>
--
--
--
-- @GetBucketLifecycle@ has the following special error:
--
--     * Error code: @TooManyConfigurations@
--
--     * Description: You are attempting to create a new configuration but have already reached the 1,000-configuration limit.
--
--     * HTTP Status Code: HTTP 400 Bad Request
module Network.AWS.S3.PutBucketMetricsConfiguration
  ( -- * Creating a Request
    putBucketMetricsConfiguration,
    PutBucketMetricsConfiguration,

    -- * Request Lenses
    pbmcExpectedBucketOwner,
    pbmcBucket,
    pbmcId,
    pbmcMetricsConfiguration,

    -- * Destructuring the Response
    putBucketMetricsConfigurationResponse,
    PutBucketMetricsConfigurationResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'putBucketMetricsConfiguration' smart constructor.
data PutBucketMetricsConfiguration = PutBucketMetricsConfiguration'
  { _pbmcExpectedBucketOwner ::
      !(Maybe Text),
    _pbmcBucket :: !BucketName,
    _pbmcId :: !Text,
    _pbmcMetricsConfiguration ::
      !MetricsConfiguration
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutBucketMetricsConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbmcExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'pbmcBucket' - The name of the bucket for which the metrics configuration is set.
--
-- * 'pbmcId' - The ID used to identify the metrics configuration.
--
-- * 'pbmcMetricsConfiguration' - Specifies the metrics configuration.
putBucketMetricsConfiguration ::
  -- | 'pbmcBucket'
  BucketName ->
  -- | 'pbmcId'
  Text ->
  -- | 'pbmcMetricsConfiguration'
  MetricsConfiguration ->
  PutBucketMetricsConfiguration
putBucketMetricsConfiguration pBucket_ pId_ pMetricsConfiguration_ =
  PutBucketMetricsConfiguration'
    { _pbmcExpectedBucketOwner =
        Nothing,
      _pbmcBucket = pBucket_,
      _pbmcId = pId_,
      _pbmcMetricsConfiguration = pMetricsConfiguration_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
pbmcExpectedBucketOwner :: Lens' PutBucketMetricsConfiguration (Maybe Text)
pbmcExpectedBucketOwner = lens _pbmcExpectedBucketOwner (\s a -> s {_pbmcExpectedBucketOwner = a})

-- | The name of the bucket for which the metrics configuration is set.
pbmcBucket :: Lens' PutBucketMetricsConfiguration BucketName
pbmcBucket = lens _pbmcBucket (\s a -> s {_pbmcBucket = a})

-- | The ID used to identify the metrics configuration.
pbmcId :: Lens' PutBucketMetricsConfiguration Text
pbmcId = lens _pbmcId (\s a -> s {_pbmcId = a})

-- | Specifies the metrics configuration.
pbmcMetricsConfiguration :: Lens' PutBucketMetricsConfiguration MetricsConfiguration
pbmcMetricsConfiguration = lens _pbmcMetricsConfiguration (\s a -> s {_pbmcMetricsConfiguration = a})

instance AWSRequest PutBucketMetricsConfiguration where
  type
    Rs PutBucketMetricsConfiguration =
      PutBucketMetricsConfigurationResponse
  request = putXML s3
  response = receiveNull PutBucketMetricsConfigurationResponse'

instance Hashable PutBucketMetricsConfiguration

instance NFData PutBucketMetricsConfiguration

instance ToElement PutBucketMetricsConfiguration where
  toElement =
    mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}MetricsConfiguration"
      . _pbmcMetricsConfiguration

instance ToHeaders PutBucketMetricsConfiguration where
  toHeaders PutBucketMetricsConfiguration' {..} =
    mconcat
      ["x-amz-expected-bucket-owner" =# _pbmcExpectedBucketOwner]

instance ToPath PutBucketMetricsConfiguration where
  toPath PutBucketMetricsConfiguration' {..} =
    mconcat ["/", toBS _pbmcBucket]

instance ToQuery PutBucketMetricsConfiguration where
  toQuery PutBucketMetricsConfiguration' {..} =
    mconcat ["id" =: _pbmcId, "metrics"]

-- | /See:/ 'putBucketMetricsConfigurationResponse' smart constructor.
data PutBucketMetricsConfigurationResponse = PutBucketMetricsConfigurationResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutBucketMetricsConfigurationResponse' with the minimum fields required to make a request.
putBucketMetricsConfigurationResponse ::
  PutBucketMetricsConfigurationResponse
putBucketMetricsConfigurationResponse =
  PutBucketMetricsConfigurationResponse'

instance NFData PutBucketMetricsConfigurationResponse
