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
-- Module      : Network.AWS.S3.GetBucketPolicyStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the policy status for an Amazon S3 bucket, indicating whether the bucket is public. In order to use this operation, you must have the @s3:GetBucketPolicyStatus@ permission. For more information about Amazon S3 permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html Specifying Permissions in a Policy> .
--
--
-- For more information about when Amazon S3 considers a bucket public, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html#access-control-block-public-access-policy-status The Meaning of "Public"> .
--
-- The following operations are related to @GetBucketPolicyStatus@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html Using Amazon S3 Block Public Access>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetPublicAccessBlock.html GetPublicAccessBlock>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutPublicAccessBlock.html PutPublicAccessBlock>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeletePublicAccessBlock.html DeletePublicAccessBlock>
module Network.AWS.S3.GetBucketPolicyStatus
  ( -- * Creating a Request
    getBucketPolicyStatus,
    GetBucketPolicyStatus,

    -- * Request Lenses
    gbpsExpectedBucketOwner,
    gbpsBucket,

    -- * Destructuring the Response
    getBucketPolicyStatusResponse,
    GetBucketPolicyStatusResponse,

    -- * Response Lenses
    gbpsrsPolicyStatus,
    gbpsrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'getBucketPolicyStatus' smart constructor.
data GetBucketPolicyStatus = GetBucketPolicyStatus'
  { _gbpsExpectedBucketOwner ::
      !(Maybe Text),
    _gbpsBucket :: !BucketName
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetBucketPolicyStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbpsExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'gbpsBucket' - The name of the Amazon S3 bucket whose policy status you want to retrieve.
getBucketPolicyStatus ::
  -- | 'gbpsBucket'
  BucketName ->
  GetBucketPolicyStatus
getBucketPolicyStatus pBucket_ =
  GetBucketPolicyStatus'
    { _gbpsExpectedBucketOwner = Nothing,
      _gbpsBucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
gbpsExpectedBucketOwner :: Lens' GetBucketPolicyStatus (Maybe Text)
gbpsExpectedBucketOwner = lens _gbpsExpectedBucketOwner (\s a -> s {_gbpsExpectedBucketOwner = a})

-- | The name of the Amazon S3 bucket whose policy status you want to retrieve.
gbpsBucket :: Lens' GetBucketPolicyStatus BucketName
gbpsBucket = lens _gbpsBucket (\s a -> s {_gbpsBucket = a})

instance AWSRequest GetBucketPolicyStatus where
  type Rs GetBucketPolicyStatus = GetBucketPolicyStatusResponse
  request = get s3
  response =
    receiveXML
      ( \s h x ->
          GetBucketPolicyStatusResponse'
            <$> (parseXML x) <*> (pure (fromEnum s))
      )

instance Hashable GetBucketPolicyStatus

instance NFData GetBucketPolicyStatus

instance ToHeaders GetBucketPolicyStatus where
  toHeaders GetBucketPolicyStatus' {..} =
    mconcat
      ["x-amz-expected-bucket-owner" =# _gbpsExpectedBucketOwner]

instance ToPath GetBucketPolicyStatus where
  toPath GetBucketPolicyStatus' {..} = mconcat ["/", toBS _gbpsBucket]

instance ToQuery GetBucketPolicyStatus where
  toQuery = const (mconcat ["policyStatus"])

-- | /See:/ 'getBucketPolicyStatusResponse' smart constructor.
data GetBucketPolicyStatusResponse = GetBucketPolicyStatusResponse'
  { _gbpsrsPolicyStatus ::
      !(Maybe PolicyStatus),
    _gbpsrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetBucketPolicyStatusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gbpsrsPolicyStatus' - The policy status for the specified bucket.
--
-- * 'gbpsrsResponseStatus' - -- | The response status code.
getBucketPolicyStatusResponse ::
  -- | 'gbpsrsResponseStatus'
  Int ->
  GetBucketPolicyStatusResponse
getBucketPolicyStatusResponse pResponseStatus_ =
  GetBucketPolicyStatusResponse'
    { _gbpsrsPolicyStatus = Nothing,
      _gbpsrsResponseStatus = pResponseStatus_
    }

-- | The policy status for the specified bucket.
gbpsrsPolicyStatus :: Lens' GetBucketPolicyStatusResponse (Maybe PolicyStatus)
gbpsrsPolicyStatus = lens _gbpsrsPolicyStatus (\s a -> s {_gbpsrsPolicyStatus = a})

-- | -- | The response status code.
gbpsrsResponseStatus :: Lens' GetBucketPolicyStatusResponse Int
gbpsrsResponseStatus = lens _gbpsrsResponseStatus (\s a -> s {_gbpsrsResponseStatus = a})

instance NFData GetBucketPolicyStatusResponse
