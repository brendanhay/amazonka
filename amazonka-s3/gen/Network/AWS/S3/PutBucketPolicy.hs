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
-- Module      : Network.AWS.S3.PutBucketPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies an Amazon S3 bucket policy to an Amazon S3 bucket. If you are
-- using an identity other than the root user of the AWS account that owns
-- the bucket, the calling identity must have the @PutBucketPolicy@
-- permissions on the specified bucket and belong to the bucket owner\'s
-- account in order to use this operation.
--
-- If you don\'t have @PutBucketPolicy@ permissions, Amazon S3 returns a
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
-- The following operations are related to @PutBucketPolicy@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucket.html DeleteBucket>
module Network.AWS.S3.PutBucketPolicy
  ( -- * Creating a Request
    PutBucketPolicy (..),
    newPutBucketPolicy,

    -- * Request Lenses
    putBucketPolicy_expectedBucketOwner,
    putBucketPolicy_contentMD5,
    putBucketPolicy_confirmRemoveSelfBucketAccess,
    putBucketPolicy_bucket,
    putBucketPolicy_policy,

    -- * Destructuring the Response
    PutBucketPolicyResponse (..),
    newPutBucketPolicyResponse,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newPutBucketPolicy' smart constructor.
data PutBucketPolicy = PutBucketPolicy'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Core.Text,
    -- | The MD5 hash of the request body.
    --
    -- For requests made using the AWS Command Line Interface (CLI) or AWS
    -- SDKs, this field is calculated automatically.
    contentMD5 :: Core.Maybe Core.Text,
    -- | Set this parameter to true to confirm that you want to remove your
    -- permissions to change this bucket policy in the future.
    confirmRemoveSelfBucketAccess :: Core.Maybe Core.Bool,
    -- | The name of the bucket.
    bucket :: BucketName,
    -- | The bucket policy as a JSON document.
    policy :: Core.ByteString
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutBucketPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'putBucketPolicy_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'contentMD5', 'putBucketPolicy_contentMD5' - The MD5 hash of the request body.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS
-- SDKs, this field is calculated automatically.
--
-- 'confirmRemoveSelfBucketAccess', 'putBucketPolicy_confirmRemoveSelfBucketAccess' - Set this parameter to true to confirm that you want to remove your
-- permissions to change this bucket policy in the future.
--
-- 'bucket', 'putBucketPolicy_bucket' - The name of the bucket.
--
-- 'policy', 'putBucketPolicy_policy' - The bucket policy as a JSON document.
newPutBucketPolicy ::
  -- | 'bucket'
  BucketName ->
  -- | 'policy'
  Core.ByteString ->
  PutBucketPolicy
newPutBucketPolicy pBucket_ pPolicy_ =
  PutBucketPolicy'
    { expectedBucketOwner =
        Core.Nothing,
      contentMD5 = Core.Nothing,
      confirmRemoveSelfBucketAccess = Core.Nothing,
      bucket = pBucket_,
      policy = pPolicy_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
putBucketPolicy_expectedBucketOwner :: Lens.Lens' PutBucketPolicy (Core.Maybe Core.Text)
putBucketPolicy_expectedBucketOwner = Lens.lens (\PutBucketPolicy' {expectedBucketOwner} -> expectedBucketOwner) (\s@PutBucketPolicy' {} a -> s {expectedBucketOwner = a} :: PutBucketPolicy)

-- | The MD5 hash of the request body.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS
-- SDKs, this field is calculated automatically.
putBucketPolicy_contentMD5 :: Lens.Lens' PutBucketPolicy (Core.Maybe Core.Text)
putBucketPolicy_contentMD5 = Lens.lens (\PutBucketPolicy' {contentMD5} -> contentMD5) (\s@PutBucketPolicy' {} a -> s {contentMD5 = a} :: PutBucketPolicy)

-- | Set this parameter to true to confirm that you want to remove your
-- permissions to change this bucket policy in the future.
putBucketPolicy_confirmRemoveSelfBucketAccess :: Lens.Lens' PutBucketPolicy (Core.Maybe Core.Bool)
putBucketPolicy_confirmRemoveSelfBucketAccess = Lens.lens (\PutBucketPolicy' {confirmRemoveSelfBucketAccess} -> confirmRemoveSelfBucketAccess) (\s@PutBucketPolicy' {} a -> s {confirmRemoveSelfBucketAccess = a} :: PutBucketPolicy)

-- | The name of the bucket.
putBucketPolicy_bucket :: Lens.Lens' PutBucketPolicy BucketName
putBucketPolicy_bucket = Lens.lens (\PutBucketPolicy' {bucket} -> bucket) (\s@PutBucketPolicy' {} a -> s {bucket = a} :: PutBucketPolicy)

-- | The bucket policy as a JSON document.
putBucketPolicy_policy :: Lens.Lens' PutBucketPolicy Core.ByteString
putBucketPolicy_policy = Lens.lens (\PutBucketPolicy' {policy} -> policy) (\s@PutBucketPolicy' {} a -> s {policy = a} :: PutBucketPolicy)

instance Core.AWSRequest PutBucketPolicy where
  type
    AWSResponse PutBucketPolicy =
      PutBucketPolicyResponse
  request =
    Request.contentMD5Header
      Core.. Request.putBody defaultService
  response =
    Response.receiveNull PutBucketPolicyResponse'

instance Core.Hashable PutBucketPolicy

instance Core.NFData PutBucketPolicy

instance Core.ToBody PutBucketPolicy where
  toBody PutBucketPolicy' {..} = Core.toBody policy

instance Core.ToHeaders PutBucketPolicy where
  toHeaders PutBucketPolicy' {..} =
    Core.mconcat
      [ "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner,
        "Content-MD5" Core.=# contentMD5,
        "x-amz-confirm-remove-self-bucket-access"
          Core.=# confirmRemoveSelfBucketAccess
      ]

instance Core.ToPath PutBucketPolicy where
  toPath PutBucketPolicy' {..} =
    Core.mconcat ["/", Core.toBS bucket]

instance Core.ToQuery PutBucketPolicy where
  toQuery = Core.const (Core.mconcat ["policy"])

-- | /See:/ 'newPutBucketPolicyResponse' smart constructor.
data PutBucketPolicyResponse = PutBucketPolicyResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutBucketPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutBucketPolicyResponse ::
  PutBucketPolicyResponse
newPutBucketPolicyResponse = PutBucketPolicyResponse'

instance Core.NFData PutBucketPolicyResponse
