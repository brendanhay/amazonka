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
-- Module      : Network.AWS.S3.GetBucketAnalyticsConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This implementation of the GET operation returns an analytics configuration (identified by the analytics configuration ID) from the bucket.
--
--
-- To use this operation, you must have permissions to perform the @s3:GetAnalyticsConfiguration@ action. The bucket owner has this permission by default. The bucket owner can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- For information about Amazon S3 analytics feature, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/analytics-storage-class.html Amazon S3 Analytics – Storage Class Analysis> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- __Related Resources__
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketAnalyticsConfiguration.html DeleteBucketAnalyticsConfiguration>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketAnalyticsConfigurations.html ListBucketAnalyticsConfigurations>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketAnalyticsConfiguration.html PutBucketAnalyticsConfiguration>
module Network.AWS.S3.GetBucketAnalyticsConfiguration
  ( -- * Creating a Request
    getBucketAnalyticsConfiguration,
    GetBucketAnalyticsConfiguration,

    -- * Request Lenses
    getExpectedBucketOwner,
    getBucket,
    getId,

    -- * Destructuring the Response
    getBucketAnalyticsConfigurationResponse,
    GetBucketAnalyticsConfigurationResponse,

    -- * Response Lenses
    gbacrsAnalyticsConfiguration,
    gbacrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'getBucketAnalyticsConfiguration' smart constructor.
data GetBucketAnalyticsConfiguration = GetBucketAnalyticsConfiguration'
  { _getExpectedBucketOwner ::
      !(Maybe Text),
    _getBucket :: !BucketName,
    _getId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetBucketAnalyticsConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'getExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'getBucket' - The name of the bucket from which an analytics configuration is retrieved.
--
-- * 'getId' - The ID that identifies the analytics configuration.
getBucketAnalyticsConfiguration ::
  -- | 'getBucket'
  BucketName ->
  -- | 'getId'
  Text ->
  GetBucketAnalyticsConfiguration
getBucketAnalyticsConfiguration pBucket_ pId_ =
  GetBucketAnalyticsConfiguration'
    { _getExpectedBucketOwner =
        Nothing,
      _getBucket = pBucket_,
      _getId = pId_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
getExpectedBucketOwner :: Lens' GetBucketAnalyticsConfiguration (Maybe Text)
getExpectedBucketOwner = lens _getExpectedBucketOwner (\s a -> s {_getExpectedBucketOwner = a})

-- | The name of the bucket from which an analytics configuration is retrieved.
getBucket :: Lens' GetBucketAnalyticsConfiguration BucketName
getBucket = lens _getBucket (\s a -> s {_getBucket = a})

-- | The ID that identifies the analytics configuration.
getId :: Lens' GetBucketAnalyticsConfiguration Text
getId = lens _getId (\s a -> s {_getId = a})

instance AWSRequest GetBucketAnalyticsConfiguration where
  type
    Rs GetBucketAnalyticsConfiguration =
      GetBucketAnalyticsConfigurationResponse
  request = get s3
  response =
    receiveXML
      ( \s h x ->
          GetBucketAnalyticsConfigurationResponse'
            <$> (parseXML x) <*> (pure (fromEnum s))
      )

instance Hashable GetBucketAnalyticsConfiguration

instance NFData GetBucketAnalyticsConfiguration

instance ToHeaders GetBucketAnalyticsConfiguration where
  toHeaders GetBucketAnalyticsConfiguration' {..} =
    mconcat
      ["x-amz-expected-bucket-owner" =# _getExpectedBucketOwner]

instance ToPath GetBucketAnalyticsConfiguration where
  toPath GetBucketAnalyticsConfiguration' {..} =
    mconcat ["/", toBS _getBucket]

instance ToQuery GetBucketAnalyticsConfiguration where
  toQuery GetBucketAnalyticsConfiguration' {..} =
    mconcat ["id" =: _getId, "analytics"]

-- | /See:/ 'getBucketAnalyticsConfigurationResponse' smart constructor.
data GetBucketAnalyticsConfigurationResponse = GetBucketAnalyticsConfigurationResponse'
  { _gbacrsAnalyticsConfiguration ::
      !( Maybe
           AnalyticsConfiguration
       ),
    _gbacrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetBucketAnalyticsConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbacrsAnalyticsConfiguration' - The configuration and any analyses for the analytics filter.
--
-- * 'gbacrsResponseStatus' - -- | The response status code.
getBucketAnalyticsConfigurationResponse ::
  -- | 'gbacrsResponseStatus'
  Int ->
  GetBucketAnalyticsConfigurationResponse
getBucketAnalyticsConfigurationResponse pResponseStatus_ =
  GetBucketAnalyticsConfigurationResponse'
    { _gbacrsAnalyticsConfiguration =
        Nothing,
      _gbacrsResponseStatus = pResponseStatus_
    }

-- | The configuration and any analyses for the analytics filter.
gbacrsAnalyticsConfiguration :: Lens' GetBucketAnalyticsConfigurationResponse (Maybe AnalyticsConfiguration)
gbacrsAnalyticsConfiguration = lens _gbacrsAnalyticsConfiguration (\s a -> s {_gbacrsAnalyticsConfiguration = a})

-- | -- | The response status code.
gbacrsResponseStatus :: Lens' GetBucketAnalyticsConfigurationResponse Int
gbacrsResponseStatus = lens _gbacrsResponseStatus (\s a -> s {_gbacrsResponseStatus = a})

instance NFData GetBucketAnalyticsConfigurationResponse
