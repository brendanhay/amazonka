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
-- Module      : Network.AWS.S3.DeleteBucketPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This implementation of the DELETE operation uses the policy subresource to delete the policy of a specified bucket. If you are using an identity other than the root user of the AWS account that owns the bucket, the calling identity must have the @DeleteBucketPolicy@ permissions on the specified bucket and belong to the bucket owner's account to use this operation.
--
--
-- If you don't have @DeleteBucketPolicy@ permissions, Amazon S3 returns a @403 Access Denied@ error. If you have the correct permissions, but you're not using an identity that belongs to the bucket owner's account, Amazon S3 returns a @405 Method Not Allowed@ error.
--
-- /Important:/ As a security precaution, the root user of the AWS account that owns a bucket can always use this operation, even if the policy explicitly denies the root user the ability to perform this action.
--
-- For more information about bucket policies, see < https://docs.aws.amazon.com/AmazonS3/latest/dev/using-iam-policies.html Using Bucket Policies and UserPolicies> .
--
-- The following operations are related to @DeleteBucketPolicy@
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteObject.html DeleteObject>
module Network.AWS.S3.DeleteBucketPolicy
  ( -- * Creating a Request
    deleteBucketPolicy,
    DeleteBucketPolicy,

    -- * Request Lenses
    dbpExpectedBucketOwner,
    dbpBucket,

    -- * Destructuring the Response
    deleteBucketPolicyResponse,
    DeleteBucketPolicyResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'deleteBucketPolicy' smart constructor.
data DeleteBucketPolicy = DeleteBucketPolicy'
  { _dbpExpectedBucketOwner ::
      !(Maybe Text),
    _dbpBucket :: !BucketName
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteBucketPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dbpExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'dbpBucket' - The bucket name.
deleteBucketPolicy ::
  -- | 'dbpBucket'
  BucketName ->
  DeleteBucketPolicy
deleteBucketPolicy pBucket_ =
  DeleteBucketPolicy'
    { _dbpExpectedBucketOwner = Nothing,
      _dbpBucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
dbpExpectedBucketOwner :: Lens' DeleteBucketPolicy (Maybe Text)
dbpExpectedBucketOwner = lens _dbpExpectedBucketOwner (\s a -> s {_dbpExpectedBucketOwner = a})

-- | The bucket name.
dbpBucket :: Lens' DeleteBucketPolicy BucketName
dbpBucket = lens _dbpBucket (\s a -> s {_dbpBucket = a})

instance AWSRequest DeleteBucketPolicy where
  type Rs DeleteBucketPolicy = DeleteBucketPolicyResponse
  request = delete s3
  response = receiveNull DeleteBucketPolicyResponse'

instance Hashable DeleteBucketPolicy

instance NFData DeleteBucketPolicy

instance ToHeaders DeleteBucketPolicy where
  toHeaders DeleteBucketPolicy' {..} =
    mconcat
      ["x-amz-expected-bucket-owner" =# _dbpExpectedBucketOwner]

instance ToPath DeleteBucketPolicy where
  toPath DeleteBucketPolicy' {..} = mconcat ["/", toBS _dbpBucket]

instance ToQuery DeleteBucketPolicy where
  toQuery = const (mconcat ["policy"])

-- | /See:/ 'deleteBucketPolicyResponse' smart constructor.
data DeleteBucketPolicyResponse = DeleteBucketPolicyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteBucketPolicyResponse' with the minimum fields required to make a request.
deleteBucketPolicyResponse ::
  DeleteBucketPolicyResponse
deleteBucketPolicyResponse = DeleteBucketPolicyResponse'

instance NFData DeleteBucketPolicyResponse
