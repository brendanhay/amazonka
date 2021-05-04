{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the policy of a specified bucket. If you are using an identity
-- other than the root user of the AWS account that owns the bucket, the
-- calling identity must have the @GetBucketPolicy@ permissions on the
-- specified bucket and belong to the bucket owner\'s account in order to
-- use this operation.
--
-- If you don\'t have @GetBucketPolicy@ permissions, Amazon S3 returns a
-- @403 Access Denied@ error. If you have the correct permissions, but
-- you\'re not using an identity that belongs to the bucket owner\'s
-- account, Amazon S3 returns a @405 Method Not Allowed@ error.
--
-- As a security precaution, the root user of the AWS account that owns a
-- bucket can always use this operation, even if the policy explicitly
-- denies the root user the ability to perform this action.
--
-- For more information about bucket policies, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-iam-policies.html Using Bucket Policies and User Policies>.
--
-- The following operation is related to @GetBucketPolicy@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
module Network.AWS.S3.GetBucketPolicy
  ( -- * Creating a Request
    GetBucketPolicy (..),
    newGetBucketPolicy,

    -- * Request Lenses
    getBucketPolicy_expectedBucketOwner,
    getBucketPolicy_bucket,

    -- * Destructuring the Response
    GetBucketPolicyResponse (..),
    newGetBucketPolicyResponse,

    -- * Response Lenses
    getBucketPolicyResponse_httpStatus,
    getBucketPolicyResponse_policy,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newGetBucketPolicy' smart constructor.
data GetBucketPolicy = GetBucketPolicy'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The bucket name for which to get the bucket policy.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetBucketPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'getBucketPolicy_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'bucket', 'getBucketPolicy_bucket' - The bucket name for which to get the bucket policy.
newGetBucketPolicy ::
  -- | 'bucket'
  BucketName ->
  GetBucketPolicy
newGetBucketPolicy pBucket_ =
  GetBucketPolicy'
    { expectedBucketOwner =
        Prelude.Nothing,
      bucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
getBucketPolicy_expectedBucketOwner :: Lens.Lens' GetBucketPolicy (Prelude.Maybe Prelude.Text)
getBucketPolicy_expectedBucketOwner = Lens.lens (\GetBucketPolicy' {expectedBucketOwner} -> expectedBucketOwner) (\s@GetBucketPolicy' {} a -> s {expectedBucketOwner = a} :: GetBucketPolicy)

-- | The bucket name for which to get the bucket policy.
getBucketPolicy_bucket :: Lens.Lens' GetBucketPolicy BucketName
getBucketPolicy_bucket = Lens.lens (\GetBucketPolicy' {bucket} -> bucket) (\s@GetBucketPolicy' {} a -> s {bucket = a} :: GetBucketPolicy)

instance Prelude.AWSRequest GetBucketPolicy where
  type Rs GetBucketPolicy = GetBucketPolicyResponse
  request = Request.get defaultService
  response =
    Response.receiveBytes
      ( \s h x ->
          GetBucketPolicyResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Prelude.pure x)
      )

instance Prelude.Hashable GetBucketPolicy

instance Prelude.NFData GetBucketPolicy

instance Prelude.ToHeaders GetBucketPolicy where
  toHeaders GetBucketPolicy' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Prelude.=# expectedBucketOwner
      ]

instance Prelude.ToPath GetBucketPolicy where
  toPath GetBucketPolicy' {..} =
    Prelude.mconcat ["/", Prelude.toBS bucket]

instance Prelude.ToQuery GetBucketPolicy where
  toQuery = Prelude.const (Prelude.mconcat ["policy"])

-- | /See:/ 'newGetBucketPolicyResponse' smart constructor.
data GetBucketPolicyResponse = GetBucketPolicyResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The bucket policy as a JSON document.
    policy :: Prelude.ByteString
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetBucketPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getBucketPolicyResponse_httpStatus' - The response's http status code.
--
-- 'policy', 'getBucketPolicyResponse_policy' - The bucket policy as a JSON document.
newGetBucketPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'policy'
  Prelude.ByteString ->
  GetBucketPolicyResponse
newGetBucketPolicyResponse pHttpStatus_ pPolicy_ =
  GetBucketPolicyResponse'
    { httpStatus = pHttpStatus_,
      policy = pPolicy_
    }

-- | The response's http status code.
getBucketPolicyResponse_httpStatus :: Lens.Lens' GetBucketPolicyResponse Prelude.Int
getBucketPolicyResponse_httpStatus = Lens.lens (\GetBucketPolicyResponse' {httpStatus} -> httpStatus) (\s@GetBucketPolicyResponse' {} a -> s {httpStatus = a} :: GetBucketPolicyResponse)

-- | The bucket policy as a JSON document.
getBucketPolicyResponse_policy :: Lens.Lens' GetBucketPolicyResponse Prelude.ByteString
getBucketPolicyResponse_policy = Lens.lens (\GetBucketPolicyResponse' {policy} -> policy) (\s@GetBucketPolicyResponse' {} a -> s {policy = a} :: GetBucketPolicyResponse)

instance Prelude.NFData GetBucketPolicyResponse
