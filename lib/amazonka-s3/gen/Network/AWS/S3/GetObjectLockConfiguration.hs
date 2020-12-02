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
-- Module      : Network.AWS.S3.GetObjectLockConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the Object Lock configuration for a bucket. The rule specified in the Object Lock configuration will be applied by default to every new object placed in the specified bucket. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Locking Objects> .
module Network.AWS.S3.GetObjectLockConfiguration
  ( -- * Creating a Request
    getObjectLockConfiguration,
    GetObjectLockConfiguration,

    -- * Request Lenses
    golcExpectedBucketOwner,
    golcBucket,

    -- * Destructuring the Response
    getObjectLockConfigurationResponse,
    GetObjectLockConfigurationResponse,

    -- * Response Lenses
    golcrsObjectLockConfiguration,
    golcrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'getObjectLockConfiguration' smart constructor.
data GetObjectLockConfiguration = GetObjectLockConfiguration'
  { _golcExpectedBucketOwner ::
      !(Maybe Text),
    _golcBucket :: !BucketName
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetObjectLockConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'golcExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'golcBucket' - The bucket whose Object Lock configuration you want to retrieve. When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
getObjectLockConfiguration ::
  -- | 'golcBucket'
  BucketName ->
  GetObjectLockConfiguration
getObjectLockConfiguration pBucket_ =
  GetObjectLockConfiguration'
    { _golcExpectedBucketOwner = Nothing,
      _golcBucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
golcExpectedBucketOwner :: Lens' GetObjectLockConfiguration (Maybe Text)
golcExpectedBucketOwner = lens _golcExpectedBucketOwner (\s a -> s {_golcExpectedBucketOwner = a})

-- | The bucket whose Object Lock configuration you want to retrieve. When using this API with an access point, you must direct requests to the access point hostname. The access point hostname takes the form /AccessPointName/ -/AccountId/ .s3-accesspoint./Region/ .amazonaws.com. When using this operation with an access point through the AWS SDKs, you provide the access point ARN in place of the bucket name. For more information about access point ARNs, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points> in the /Amazon Simple Storage Service Developer Guide/ .
golcBucket :: Lens' GetObjectLockConfiguration BucketName
golcBucket = lens _golcBucket (\s a -> s {_golcBucket = a})

instance AWSRequest GetObjectLockConfiguration where
  type
    Rs GetObjectLockConfiguration =
      GetObjectLockConfigurationResponse
  request = get s3
  response =
    receiveXML
      ( \s h x ->
          GetObjectLockConfigurationResponse'
            <$> (parseXML x) <*> (pure (fromEnum s))
      )

instance Hashable GetObjectLockConfiguration

instance NFData GetObjectLockConfiguration

instance ToHeaders GetObjectLockConfiguration where
  toHeaders GetObjectLockConfiguration' {..} =
    mconcat
      ["x-amz-expected-bucket-owner" =# _golcExpectedBucketOwner]

instance ToPath GetObjectLockConfiguration where
  toPath GetObjectLockConfiguration' {..} =
    mconcat ["/", toBS _golcBucket]

instance ToQuery GetObjectLockConfiguration where
  toQuery = const (mconcat ["object-lock"])

-- | /See:/ 'getObjectLockConfigurationResponse' smart constructor.
data GetObjectLockConfigurationResponse = GetObjectLockConfigurationResponse'
  { _golcrsObjectLockConfiguration ::
      !( Maybe
           ObjectLockConfiguration
       ),
    _golcrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetObjectLockConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'golcrsObjectLockConfiguration' - The specified bucket's Object Lock configuration.
--
-- * 'golcrsResponseStatus' - -- | The response status code.
getObjectLockConfigurationResponse ::
  -- | 'golcrsResponseStatus'
  Int ->
  GetObjectLockConfigurationResponse
getObjectLockConfigurationResponse pResponseStatus_ =
  GetObjectLockConfigurationResponse'
    { _golcrsObjectLockConfiguration =
        Nothing,
      _golcrsResponseStatus = pResponseStatus_
    }

-- | The specified bucket's Object Lock configuration.
golcrsObjectLockConfiguration :: Lens' GetObjectLockConfigurationResponse (Maybe ObjectLockConfiguration)
golcrsObjectLockConfiguration = lens _golcrsObjectLockConfiguration (\s a -> s {_golcrsObjectLockConfiguration = a})

-- | -- | The response status code.
golcrsResponseStatus :: Lens' GetObjectLockConfigurationResponse Int
golcrsResponseStatus = lens _golcrsResponseStatus (\s a -> s {_golcrsResponseStatus = a})

instance NFData GetObjectLockConfigurationResponse
