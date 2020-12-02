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
-- Module      : Network.AWS.S3.GetBucketAccelerateConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This implementation of the GET operation uses the @accelerate@ subresource to return the Transfer Acceleration state of a bucket, which is either @Enabled@ or @Suspended@ . Amazon S3 Transfer Acceleration is a bucket-level feature that enables you to perform faster data transfers to and from Amazon S3.
--
--
-- To use this operation, you must have permission to perform the @s3:GetAccelerateConfiguration@ action. The bucket owner has this permission by default. The bucket owner can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to your Amazon S3 Resources> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- You set the Transfer Acceleration state of an existing bucket to @Enabled@ or @Suspended@ by using the <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketAccelerateConfiguration.html PutBucketAccelerateConfiguration> operation.
--
-- A GET @accelerate@ request does not return a state value for a bucket that has no transfer acceleration state. A bucket has no Transfer Acceleration state if a state has never been set on the bucket.
--
-- For more information about transfer acceleration, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/transfer-acceleration.html Transfer Acceleration> in the Amazon Simple Storage Service Developer Guide.
--
-- __Related Resources__
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketAccelerateConfiguration.html PutBucketAccelerateConfiguration>
module Network.AWS.S3.GetBucketAccelerateConfiguration
  ( -- * Creating a Request
    getBucketAccelerateConfiguration,
    GetBucketAccelerateConfiguration,

    -- * Request Lenses
    gbacExpectedBucketOwner,
    gbacBucket,

    -- * Destructuring the Response
    getBucketAccelerateConfigurationResponse,
    GetBucketAccelerateConfigurationResponse,

    -- * Response Lenses
    grsStatus,
    grsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'getBucketAccelerateConfiguration' smart constructor.
data GetBucketAccelerateConfiguration = GetBucketAccelerateConfiguration'
  { _gbacExpectedBucketOwner ::
      !(Maybe Text),
    _gbacBucket ::
      !BucketName
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetBucketAccelerateConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbacExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'gbacBucket' - The name of the bucket for which the accelerate configuration is retrieved.
getBucketAccelerateConfiguration ::
  -- | 'gbacBucket'
  BucketName ->
  GetBucketAccelerateConfiguration
getBucketAccelerateConfiguration pBucket_ =
  GetBucketAccelerateConfiguration'
    { _gbacExpectedBucketOwner =
        Nothing,
      _gbacBucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
gbacExpectedBucketOwner :: Lens' GetBucketAccelerateConfiguration (Maybe Text)
gbacExpectedBucketOwner = lens _gbacExpectedBucketOwner (\s a -> s {_gbacExpectedBucketOwner = a})

-- | The name of the bucket for which the accelerate configuration is retrieved.
gbacBucket :: Lens' GetBucketAccelerateConfiguration BucketName
gbacBucket = lens _gbacBucket (\s a -> s {_gbacBucket = a})

instance AWSRequest GetBucketAccelerateConfiguration where
  type
    Rs GetBucketAccelerateConfiguration =
      GetBucketAccelerateConfigurationResponse
  request = get s3
  response =
    receiveXML
      ( \s h x ->
          GetBucketAccelerateConfigurationResponse'
            <$> (x .@? "Status") <*> (pure (fromEnum s))
      )

instance Hashable GetBucketAccelerateConfiguration

instance NFData GetBucketAccelerateConfiguration

instance ToHeaders GetBucketAccelerateConfiguration where
  toHeaders GetBucketAccelerateConfiguration' {..} =
    mconcat
      ["x-amz-expected-bucket-owner" =# _gbacExpectedBucketOwner]

instance ToPath GetBucketAccelerateConfiguration where
  toPath GetBucketAccelerateConfiguration' {..} =
    mconcat ["/", toBS _gbacBucket]

instance ToQuery GetBucketAccelerateConfiguration where
  toQuery = const (mconcat ["accelerate"])

-- | /See:/ 'getBucketAccelerateConfigurationResponse' smart constructor.
data GetBucketAccelerateConfigurationResponse = GetBucketAccelerateConfigurationResponse'
  { _grsStatus ::
      !( Maybe
           BucketAccelerateStatus
       ),
    _grsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetBucketAccelerateConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grsStatus' - The accelerate configuration of the bucket.
--
-- * 'grsResponseStatus' - -- | The response status code.
getBucketAccelerateConfigurationResponse ::
  -- | 'grsResponseStatus'
  Int ->
  GetBucketAccelerateConfigurationResponse
getBucketAccelerateConfigurationResponse pResponseStatus_ =
  GetBucketAccelerateConfigurationResponse'
    { _grsStatus = Nothing,
      _grsResponseStatus = pResponseStatus_
    }

-- | The accelerate configuration of the bucket.
grsStatus :: Lens' GetBucketAccelerateConfigurationResponse (Maybe BucketAccelerateStatus)
grsStatus = lens _grsStatus (\s a -> s {_grsStatus = a})

-- | -- | The response status code.
grsResponseStatus :: Lens' GetBucketAccelerateConfigurationResponse Int
grsResponseStatus = lens _grsResponseStatus (\s a -> s {_grsResponseStatus = a})

instance NFData GetBucketAccelerateConfigurationResponse
