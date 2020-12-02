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
-- Module      : Network.AWS.S3.GetBucketPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the policy of a specified bucket. If you are using an identity other than the root user of the AWS account that owns the bucket, the calling identity must have the @GetBucketPolicy@ permissions on the specified bucket and belong to the bucket owner's account in order to use this operation.
--
--
-- If you don't have @GetBucketPolicy@ permissions, Amazon S3 returns a @403 Access Denied@ error. If you have the correct permissions, but you're not using an identity that belongs to the bucket owner's account, Amazon S3 returns a @405 Method Not Allowed@ error.
--
-- /Important:/ As a security precaution, the root user of the AWS account that owns a bucket can always use this operation, even if the policy explicitly denies the root user the ability to perform this action.
--
-- For more information about bucket policies, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-iam-policies.html Using Bucket Policies and User Policies> .
--
-- The following operation is related to @GetBucketPolicy@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
module Network.AWS.S3.GetBucketPolicy
  ( -- * Creating a Request
    getBucketPolicy,
    GetBucketPolicy,

    -- * Request Lenses
    gbpExpectedBucketOwner,
    gbpBucket,

    -- * Destructuring the Response
    getBucketPolicyResponse,
    GetBucketPolicyResponse,

    -- * Response Lenses
    gbprsResponseStatus,
    gbprsPolicy,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'getBucketPolicy' smart constructor.
data GetBucketPolicy = GetBucketPolicy'
  { _gbpExpectedBucketOwner ::
      !(Maybe Text),
    _gbpBucket :: !BucketName
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetBucketPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbpExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'gbpBucket' - The bucket name for which to get the bucket policy.
getBucketPolicy ::
  -- | 'gbpBucket'
  BucketName ->
  GetBucketPolicy
getBucketPolicy pBucket_ =
  GetBucketPolicy'
    { _gbpExpectedBucketOwner = Nothing,
      _gbpBucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
gbpExpectedBucketOwner :: Lens' GetBucketPolicy (Maybe Text)
gbpExpectedBucketOwner = lens _gbpExpectedBucketOwner (\s a -> s {_gbpExpectedBucketOwner = a})

-- | The bucket name for which to get the bucket policy.
gbpBucket :: Lens' GetBucketPolicy BucketName
gbpBucket = lens _gbpBucket (\s a -> s {_gbpBucket = a})

instance AWSRequest GetBucketPolicy where
  type Rs GetBucketPolicy = GetBucketPolicyResponse
  request = get s3
  response =
    receiveBytes
      ( \s h x ->
          GetBucketPolicyResponse' <$> (pure (fromEnum s)) <*> (pure x)
      )

instance Hashable GetBucketPolicy

instance NFData GetBucketPolicy

instance ToHeaders GetBucketPolicy where
  toHeaders GetBucketPolicy' {..} =
    mconcat
      ["x-amz-expected-bucket-owner" =# _gbpExpectedBucketOwner]

instance ToPath GetBucketPolicy where
  toPath GetBucketPolicy' {..} = mconcat ["/", toBS _gbpBucket]

instance ToQuery GetBucketPolicy where
  toQuery = const (mconcat ["policy"])

-- | /See:/ 'getBucketPolicyResponse' smart constructor.
data GetBucketPolicyResponse = GetBucketPolicyResponse'
  { _gbprsResponseStatus ::
      !Int,
    _gbprsPolicy :: !ByteString
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetBucketPolicyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbprsResponseStatus' - -- | The response status code.
--
-- * 'gbprsPolicy' - The bucket policy as a JSON document.
getBucketPolicyResponse ::
  -- | 'gbprsResponseStatus'
  Int ->
  -- | 'gbprsPolicy'
  ByteString ->
  GetBucketPolicyResponse
getBucketPolicyResponse pResponseStatus_ pPolicy_ =
  GetBucketPolicyResponse'
    { _gbprsResponseStatus = pResponseStatus_,
      _gbprsPolicy = pPolicy_
    }

-- | -- | The response status code.
gbprsResponseStatus :: Lens' GetBucketPolicyResponse Int
gbprsResponseStatus = lens _gbprsResponseStatus (\s a -> s {_gbprsResponseStatus = a})

-- | The bucket policy as a JSON document.
gbprsPolicy :: Lens' GetBucketPolicyResponse ByteString
gbprsPolicy = lens _gbprsPolicy (\s a -> s {_gbprsPolicy = a})

instance NFData GetBucketPolicyResponse
