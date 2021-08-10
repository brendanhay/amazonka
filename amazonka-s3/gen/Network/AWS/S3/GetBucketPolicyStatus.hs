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
-- Module      : Network.AWS.S3.GetBucketPolicyStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the policy status for an Amazon S3 bucket, indicating whether
-- the bucket is public. In order to use this operation, you must have the
-- @s3:GetBucketPolicyStatus@ permission. For more information about Amazon
-- S3 permissions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html Specifying Permissions in a Policy>.
--
-- For more information about when Amazon S3 considers a bucket public, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html#access-control-block-public-access-policy-status The Meaning of \"Public\">.
--
-- The following operations are related to @GetBucketPolicyStatus@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html Using Amazon S3 Block Public Access>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetPublicAccessBlock.html GetPublicAccessBlock>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutPublicAccessBlock.html PutPublicAccessBlock>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeletePublicAccessBlock.html DeletePublicAccessBlock>
module Network.AWS.S3.GetBucketPolicyStatus
  ( -- * Creating a Request
    GetBucketPolicyStatus (..),
    newGetBucketPolicyStatus,

    -- * Request Lenses
    getBucketPolicyStatus_expectedBucketOwner,
    getBucketPolicyStatus_bucket,

    -- * Destructuring the Response
    GetBucketPolicyStatusResponse (..),
    newGetBucketPolicyStatusResponse,

    -- * Response Lenses
    getBucketPolicyStatusResponse_policyStatus,
    getBucketPolicyStatusResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newGetBucketPolicyStatus' smart constructor.
data GetBucketPolicyStatus = GetBucketPolicyStatus'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon S3 bucket whose policy status you want to
    -- retrieve.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBucketPolicyStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'getBucketPolicyStatus_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'bucket', 'getBucketPolicyStatus_bucket' - The name of the Amazon S3 bucket whose policy status you want to
-- retrieve.
newGetBucketPolicyStatus ::
  -- | 'bucket'
  BucketName ->
  GetBucketPolicyStatus
newGetBucketPolicyStatus pBucket_ =
  GetBucketPolicyStatus'
    { expectedBucketOwner =
        Prelude.Nothing,
      bucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
getBucketPolicyStatus_expectedBucketOwner :: Lens.Lens' GetBucketPolicyStatus (Prelude.Maybe Prelude.Text)
getBucketPolicyStatus_expectedBucketOwner = Lens.lens (\GetBucketPolicyStatus' {expectedBucketOwner} -> expectedBucketOwner) (\s@GetBucketPolicyStatus' {} a -> s {expectedBucketOwner = a} :: GetBucketPolicyStatus)

-- | The name of the Amazon S3 bucket whose policy status you want to
-- retrieve.
getBucketPolicyStatus_bucket :: Lens.Lens' GetBucketPolicyStatus BucketName
getBucketPolicyStatus_bucket = Lens.lens (\GetBucketPolicyStatus' {bucket} -> bucket) (\s@GetBucketPolicyStatus' {} a -> s {bucket = a} :: GetBucketPolicyStatus)

instance Core.AWSRequest GetBucketPolicyStatus where
  type
    AWSResponse GetBucketPolicyStatus =
      GetBucketPolicyStatusResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetBucketPolicyStatusResponse'
            Prelude.<$> (Core.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBucketPolicyStatus

instance Prelude.NFData GetBucketPolicyStatus

instance Core.ToHeaders GetBucketPolicyStatus where
  toHeaders GetBucketPolicyStatus' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner
      ]

instance Core.ToPath GetBucketPolicyStatus where
  toPath GetBucketPolicyStatus' {..} =
    Prelude.mconcat ["/", Core.toBS bucket]

instance Core.ToQuery GetBucketPolicyStatus where
  toQuery =
    Prelude.const (Prelude.mconcat ["policyStatus"])

-- | /See:/ 'newGetBucketPolicyStatusResponse' smart constructor.
data GetBucketPolicyStatusResponse = GetBucketPolicyStatusResponse'
  { -- | The policy status for the specified bucket.
    policyStatus :: Prelude.Maybe PolicyStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBucketPolicyStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyStatus', 'getBucketPolicyStatusResponse_policyStatus' - The policy status for the specified bucket.
--
-- 'httpStatus', 'getBucketPolicyStatusResponse_httpStatus' - The response's http status code.
newGetBucketPolicyStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBucketPolicyStatusResponse
newGetBucketPolicyStatusResponse pHttpStatus_ =
  GetBucketPolicyStatusResponse'
    { policyStatus =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The policy status for the specified bucket.
getBucketPolicyStatusResponse_policyStatus :: Lens.Lens' GetBucketPolicyStatusResponse (Prelude.Maybe PolicyStatus)
getBucketPolicyStatusResponse_policyStatus = Lens.lens (\GetBucketPolicyStatusResponse' {policyStatus} -> policyStatus) (\s@GetBucketPolicyStatusResponse' {} a -> s {policyStatus = a} :: GetBucketPolicyStatusResponse)

-- | The response's http status code.
getBucketPolicyStatusResponse_httpStatus :: Lens.Lens' GetBucketPolicyStatusResponse Prelude.Int
getBucketPolicyStatusResponse_httpStatus = Lens.lens (\GetBucketPolicyStatusResponse' {httpStatus} -> httpStatus) (\s@GetBucketPolicyStatusResponse' {} a -> s {httpStatus = a} :: GetBucketPolicyStatusResponse)

instance Prelude.NFData GetBucketPolicyStatusResponse
