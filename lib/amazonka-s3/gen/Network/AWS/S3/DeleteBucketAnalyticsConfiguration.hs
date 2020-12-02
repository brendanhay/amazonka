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
-- Module      : Network.AWS.S3.DeleteBucketAnalyticsConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an analytics configuration for the bucket (specified by the analytics configuration ID).
--
--
-- To use this operation, you must have permissions to perform the @s3:PutAnalyticsConfiguration@ action. The bucket owner has this permission by default. The bucket owner can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
--
-- For information about the Amazon S3 analytics feature, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/analytics-storage-class.html Amazon S3 Analytics – Storage Class Analysis> .
--
-- The following operations are related to @DeleteBucketAnalyticsConfiguration@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketAnalyticsConfiguration.html GetBucketAnalyticsConfiguration>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketAnalyticsConfigurations.html ListBucketAnalyticsConfigurations>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketAnalyticsConfiguration.html PutBucketAnalyticsConfiguration>
module Network.AWS.S3.DeleteBucketAnalyticsConfiguration
  ( -- * Creating a Request
    deleteBucketAnalyticsConfiguration,
    DeleteBucketAnalyticsConfiguration,

    -- * Request Lenses
    dbacExpectedBucketOwner,
    dbacBucket,
    dbacId,

    -- * Destructuring the Response
    deleteBucketAnalyticsConfigurationResponse,
    DeleteBucketAnalyticsConfigurationResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'deleteBucketAnalyticsConfiguration' smart constructor.
data DeleteBucketAnalyticsConfiguration = DeleteBucketAnalyticsConfiguration'
  { _dbacExpectedBucketOwner ::
      !(Maybe Text),
    _dbacBucket ::
      !BucketName,
    _dbacId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteBucketAnalyticsConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbacExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'dbacBucket' - The name of the bucket from which an analytics configuration is deleted.
--
-- * 'dbacId' - The ID that identifies the analytics configuration.
deleteBucketAnalyticsConfiguration ::
  -- | 'dbacBucket'
  BucketName ->
  -- | 'dbacId'
  Text ->
  DeleteBucketAnalyticsConfiguration
deleteBucketAnalyticsConfiguration pBucket_ pId_ =
  DeleteBucketAnalyticsConfiguration'
    { _dbacExpectedBucketOwner =
        Nothing,
      _dbacBucket = pBucket_,
      _dbacId = pId_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
dbacExpectedBucketOwner :: Lens' DeleteBucketAnalyticsConfiguration (Maybe Text)
dbacExpectedBucketOwner = lens _dbacExpectedBucketOwner (\s a -> s {_dbacExpectedBucketOwner = a})

-- | The name of the bucket from which an analytics configuration is deleted.
dbacBucket :: Lens' DeleteBucketAnalyticsConfiguration BucketName
dbacBucket = lens _dbacBucket (\s a -> s {_dbacBucket = a})

-- | The ID that identifies the analytics configuration.
dbacId :: Lens' DeleteBucketAnalyticsConfiguration Text
dbacId = lens _dbacId (\s a -> s {_dbacId = a})

instance AWSRequest DeleteBucketAnalyticsConfiguration where
  type
    Rs DeleteBucketAnalyticsConfiguration =
      DeleteBucketAnalyticsConfigurationResponse
  request = delete s3
  response = receiveNull DeleteBucketAnalyticsConfigurationResponse'

instance Hashable DeleteBucketAnalyticsConfiguration

instance NFData DeleteBucketAnalyticsConfiguration

instance ToHeaders DeleteBucketAnalyticsConfiguration where
  toHeaders DeleteBucketAnalyticsConfiguration' {..} =
    mconcat
      ["x-amz-expected-bucket-owner" =# _dbacExpectedBucketOwner]

instance ToPath DeleteBucketAnalyticsConfiguration where
  toPath DeleteBucketAnalyticsConfiguration' {..} =
    mconcat ["/", toBS _dbacBucket]

instance ToQuery DeleteBucketAnalyticsConfiguration where
  toQuery DeleteBucketAnalyticsConfiguration' {..} =
    mconcat ["id" =: _dbacId, "analytics"]

-- | /See:/ 'deleteBucketAnalyticsConfigurationResponse' smart constructor.
data DeleteBucketAnalyticsConfigurationResponse = DeleteBucketAnalyticsConfigurationResponse'
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DeleteBucketAnalyticsConfigurationResponse' with the minimum fields required to make a request.
deleteBucketAnalyticsConfigurationResponse ::
  DeleteBucketAnalyticsConfigurationResponse
deleteBucketAnalyticsConfigurationResponse =
  DeleteBucketAnalyticsConfigurationResponse'

instance NFData DeleteBucketAnalyticsConfigurationResponse
