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
-- Module      : Network.AWS.S3.DeleteBucketMetricsConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a metrics configuration for the Amazon CloudWatch request metrics (specified by the metrics configuration ID) from the bucket. Note that this doesn't include the daily storage metrics.
--
--
-- To use this operation, you must have permissions to perform the @s3:PutMetricsConfiguration@ action. The bucket owner has this permission by default. The bucket owner can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
--
-- For information about CloudWatch request metrics for Amazon S3, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/cloudwatch-monitoring.html Monitoring Metrics with Amazon CloudWatch> .
--
-- The following operations are related to @DeleteBucketMetricsConfiguration@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketMetricsConfiguration.html GetBucketMetricsConfiguration>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketMetricsConfiguration.html PutBucketMetricsConfiguration>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketMetricsConfigurations.html ListBucketMetricsConfigurations>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/dev/cloudwatch-monitoring.html Monitoring Metrics with Amazon CloudWatch>
module Network.AWS.S3.DeleteBucketMetricsConfiguration
  ( -- * Creating a Request
    deleteBucketMetricsConfiguration,
    DeleteBucketMetricsConfiguration,

    -- * Request Lenses
    dbmcExpectedBucketOwner,
    dbmcBucket,
    dbmcId,

    -- * Destructuring the Response
    deleteBucketMetricsConfigurationResponse,
    DeleteBucketMetricsConfigurationResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'deleteBucketMetricsConfiguration' smart constructor.
data DeleteBucketMetricsConfiguration = DeleteBucketMetricsConfiguration'
  { _dbmcExpectedBucketOwner ::
      !(Maybe Text),
    _dbmcBucket ::
      !BucketName,
    _dbmcId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteBucketMetricsConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbmcExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'dbmcBucket' - The name of the bucket containing the metrics configuration to delete.
--
-- * 'dbmcId' - The ID used to identify the metrics configuration.
deleteBucketMetricsConfiguration ::
  -- | 'dbmcBucket'
  BucketName ->
  -- | 'dbmcId'
  Text ->
  DeleteBucketMetricsConfiguration
deleteBucketMetricsConfiguration pBucket_ pId_ =
  DeleteBucketMetricsConfiguration'
    { _dbmcExpectedBucketOwner =
        Nothing,
      _dbmcBucket = pBucket_,
      _dbmcId = pId_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
dbmcExpectedBucketOwner :: Lens' DeleteBucketMetricsConfiguration (Maybe Text)
dbmcExpectedBucketOwner = lens _dbmcExpectedBucketOwner (\s a -> s {_dbmcExpectedBucketOwner = a})

-- | The name of the bucket containing the metrics configuration to delete.
dbmcBucket :: Lens' DeleteBucketMetricsConfiguration BucketName
dbmcBucket = lens _dbmcBucket (\s a -> s {_dbmcBucket = a})

-- | The ID used to identify the metrics configuration.
dbmcId :: Lens' DeleteBucketMetricsConfiguration Text
dbmcId = lens _dbmcId (\s a -> s {_dbmcId = a})

instance AWSRequest DeleteBucketMetricsConfiguration where
  type
    Rs DeleteBucketMetricsConfiguration =
      DeleteBucketMetricsConfigurationResponse
  request = delete s3
  response = receiveNull DeleteBucketMetricsConfigurationResponse'

instance Hashable DeleteBucketMetricsConfiguration

instance NFData DeleteBucketMetricsConfiguration

instance ToHeaders DeleteBucketMetricsConfiguration where
  toHeaders DeleteBucketMetricsConfiguration' {..} =
    mconcat
      ["x-amz-expected-bucket-owner" =# _dbmcExpectedBucketOwner]

instance ToPath DeleteBucketMetricsConfiguration where
  toPath DeleteBucketMetricsConfiguration' {..} =
    mconcat ["/", toBS _dbmcBucket]

instance ToQuery DeleteBucketMetricsConfiguration where
  toQuery DeleteBucketMetricsConfiguration' {..} =
    mconcat ["id" =: _dbmcId, "metrics"]

-- | /See:/ 'deleteBucketMetricsConfigurationResponse' smart constructor.
data DeleteBucketMetricsConfigurationResponse = DeleteBucketMetricsConfigurationResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteBucketMetricsConfigurationResponse' with the minimum fields required to make a request.
deleteBucketMetricsConfigurationResponse ::
  DeleteBucketMetricsConfigurationResponse
deleteBucketMetricsConfigurationResponse =
  DeleteBucketMetricsConfigurationResponse'

instance NFData DeleteBucketMetricsConfigurationResponse
