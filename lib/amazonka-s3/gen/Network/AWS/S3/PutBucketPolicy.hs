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
-- Module      : Network.AWS.S3.PutBucketPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies an Amazon S3 bucket policy to an Amazon S3 bucket. If you are using an identity other than the root user of the AWS account that owns the bucket, the calling identity must have the @PutBucketPolicy@ permissions on the specified bucket and belong to the bucket owner's account in order to use this operation.
--
--
-- If you don't have @PutBucketPolicy@ permissions, Amazon S3 returns a @403 Access Denied@ error. If you have the correct permissions, but you're not using an identity that belongs to the bucket owner's account, Amazon S3 returns a @405 Method Not Allowed@ error.
--
-- /Important:/ As a security precaution, the root user of the AWS account that owns a bucket can always use this operation, even if the policy explicitly denies the root user the ability to perform this action.
--
-- For more information about bucket policies, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-iam-policies.html Using Bucket Policies and User Policies> .
--
-- The following operations are related to @PutBucketPolicy@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket>
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucket.html DeleteBucket>
module Network.AWS.S3.PutBucketPolicy
  ( -- * Creating a Request
    putBucketPolicy,
    PutBucketPolicy,

    -- * Request Lenses
    pbpConfirmRemoveSelfBucketAccess,
    pbpContentMD5,
    pbpExpectedBucketOwner,
    pbpBucket,
    pbpPolicy,

    -- * Destructuring the Response
    putBucketPolicyResponse,
    PutBucketPolicyResponse,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

-- | /See:/ 'putBucketPolicy' smart constructor.
data PutBucketPolicy = PutBucketPolicy'
  { _pbpConfirmRemoveSelfBucketAccess ::
      !(Maybe Bool),
    _pbpContentMD5 :: !(Maybe Text),
    _pbpExpectedBucketOwner :: !(Maybe Text),
    _pbpBucket :: !BucketName,
    _pbpPolicy :: !ByteString
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutBucketPolicy' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pbpConfirmRemoveSelfBucketAccess' - Set this parameter to true to confirm that you want to remove your permissions to change this bucket policy in the future.
--
-- * 'pbpContentMD5' - The MD5 hash of the request body. For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- * 'pbpExpectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- * 'pbpBucket' - The name of the bucket.
--
-- * 'pbpPolicy' - The bucket policy as a JSON document.
putBucketPolicy ::
  -- | 'pbpBucket'
  BucketName ->
  -- | 'pbpPolicy'
  ByteString ->
  PutBucketPolicy
putBucketPolicy pBucket_ pPolicy_ =
  PutBucketPolicy'
    { _pbpConfirmRemoveSelfBucketAccess = Nothing,
      _pbpContentMD5 = Nothing,
      _pbpExpectedBucketOwner = Nothing,
      _pbpBucket = pBucket_,
      _pbpPolicy = pPolicy_
    }

-- | Set this parameter to true to confirm that you want to remove your permissions to change this bucket policy in the future.
pbpConfirmRemoveSelfBucketAccess :: Lens' PutBucketPolicy (Maybe Bool)
pbpConfirmRemoveSelfBucketAccess = lens _pbpConfirmRemoveSelfBucketAccess (\s a -> s {_pbpConfirmRemoveSelfBucketAccess = a})

-- | The MD5 hash of the request body. For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
pbpContentMD5 :: Lens' PutBucketPolicy (Maybe Text)
pbpContentMD5 = lens _pbpContentMD5 (\s a -> s {_pbpContentMD5 = a})

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
pbpExpectedBucketOwner :: Lens' PutBucketPolicy (Maybe Text)
pbpExpectedBucketOwner = lens _pbpExpectedBucketOwner (\s a -> s {_pbpExpectedBucketOwner = a})

-- | The name of the bucket.
pbpBucket :: Lens' PutBucketPolicy BucketName
pbpBucket = lens _pbpBucket (\s a -> s {_pbpBucket = a})

-- | The bucket policy as a JSON document.
pbpPolicy :: Lens' PutBucketPolicy ByteString
pbpPolicy = lens _pbpPolicy (\s a -> s {_pbpPolicy = a})

instance AWSRequest PutBucketPolicy where
  type Rs PutBucketPolicy = PutBucketPolicyResponse
  request = contentMD5Header . putBody s3
  response = receiveNull PutBucketPolicyResponse'

instance Hashable PutBucketPolicy

instance NFData PutBucketPolicy

instance ToBody PutBucketPolicy where
  toBody = toBody . _pbpPolicy

instance ToHeaders PutBucketPolicy where
  toHeaders PutBucketPolicy' {..} =
    mconcat
      [ "x-amz-confirm-remove-self-bucket-access"
          =# _pbpConfirmRemoveSelfBucketAccess,
        "Content-MD5" =# _pbpContentMD5,
        "x-amz-expected-bucket-owner" =# _pbpExpectedBucketOwner
      ]

instance ToPath PutBucketPolicy where
  toPath PutBucketPolicy' {..} = mconcat ["/", toBS _pbpBucket]

instance ToQuery PutBucketPolicy where
  toQuery = const (mconcat ["policy"])

-- | /See:/ 'putBucketPolicyResponse' smart constructor.
data PutBucketPolicyResponse = PutBucketPolicyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutBucketPolicyResponse' with the minimum fields required to make a request.
putBucketPolicyResponse ::
  PutBucketPolicyResponse
putBucketPolicyResponse = PutBucketPolicyResponse'

instance NFData PutBucketPolicyResponse
