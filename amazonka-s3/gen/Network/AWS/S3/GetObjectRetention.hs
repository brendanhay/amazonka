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
-- Module      : Network.AWS.S3.GetObjectRetention
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an object\'s retention settings. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Locking Objects>.
--
-- This action is not supported by Amazon S3 on Outposts.
module Network.AWS.S3.GetObjectRetention
  ( -- * Creating a Request
    GetObjectRetention (..),
    newGetObjectRetention,

    -- * Request Lenses
    getObjectRetention_expectedBucketOwner,
    getObjectRetention_versionId,
    getObjectRetention_requestPayer,
    getObjectRetention_bucket,
    getObjectRetention_key,

    -- * Destructuring the Response
    GetObjectRetentionResponse (..),
    newGetObjectRetentionResponse,

    -- * Response Lenses
    getObjectRetentionResponse_retention,
    getObjectRetentionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newGetObjectRetention' smart constructor.
data GetObjectRetention = GetObjectRetention'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Core.Text,
    -- | The version ID for the object whose retention settings you want to
    -- retrieve.
    versionId :: Core.Maybe ObjectVersionId,
    requestPayer :: Core.Maybe RequestPayer,
    -- | The bucket name containing the object whose retention settings you want
    -- to retrieve.
    --
    -- When using this API with an access point, you must direct requests to
    -- the access point hostname. The access point hostname takes the form
    -- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
    -- When using this operation with an access point through the AWS SDKs, you
    -- provide the access point ARN in place of the bucket name. For more
    -- information about access point ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points>
    -- in the /Amazon Simple Storage Service Developer Guide/.
    bucket :: BucketName,
    -- | The key name for the object whose retention settings you want to
    -- retrieve.
    key :: ObjectKey
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetObjectRetention' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'getObjectRetention_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'versionId', 'getObjectRetention_versionId' - The version ID for the object whose retention settings you want to
-- retrieve.
--
-- 'requestPayer', 'getObjectRetention_requestPayer' - Undocumented member.
--
-- 'bucket', 'getObjectRetention_bucket' - The bucket name containing the object whose retention settings you want
-- to retrieve.
--
-- When using this API with an access point, you must direct requests to
-- the access point hostname. The access point hostname takes the form
-- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
-- When using this operation with an access point through the AWS SDKs, you
-- provide the access point ARN in place of the bucket name. For more
-- information about access point ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- 'key', 'getObjectRetention_key' - The key name for the object whose retention settings you want to
-- retrieve.
newGetObjectRetention ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  GetObjectRetention
newGetObjectRetention pBucket_ pKey_ =
  GetObjectRetention'
    { expectedBucketOwner =
        Core.Nothing,
      versionId = Core.Nothing,
      requestPayer = Core.Nothing,
      bucket = pBucket_,
      key = pKey_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
getObjectRetention_expectedBucketOwner :: Lens.Lens' GetObjectRetention (Core.Maybe Core.Text)
getObjectRetention_expectedBucketOwner = Lens.lens (\GetObjectRetention' {expectedBucketOwner} -> expectedBucketOwner) (\s@GetObjectRetention' {} a -> s {expectedBucketOwner = a} :: GetObjectRetention)

-- | The version ID for the object whose retention settings you want to
-- retrieve.
getObjectRetention_versionId :: Lens.Lens' GetObjectRetention (Core.Maybe ObjectVersionId)
getObjectRetention_versionId = Lens.lens (\GetObjectRetention' {versionId} -> versionId) (\s@GetObjectRetention' {} a -> s {versionId = a} :: GetObjectRetention)

-- | Undocumented member.
getObjectRetention_requestPayer :: Lens.Lens' GetObjectRetention (Core.Maybe RequestPayer)
getObjectRetention_requestPayer = Lens.lens (\GetObjectRetention' {requestPayer} -> requestPayer) (\s@GetObjectRetention' {} a -> s {requestPayer = a} :: GetObjectRetention)

-- | The bucket name containing the object whose retention settings you want
-- to retrieve.
--
-- When using this API with an access point, you must direct requests to
-- the access point hostname. The access point hostname takes the form
-- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
-- When using this operation with an access point through the AWS SDKs, you
-- provide the access point ARN in place of the bucket name. For more
-- information about access point ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points>
-- in the /Amazon Simple Storage Service Developer Guide/.
getObjectRetention_bucket :: Lens.Lens' GetObjectRetention BucketName
getObjectRetention_bucket = Lens.lens (\GetObjectRetention' {bucket} -> bucket) (\s@GetObjectRetention' {} a -> s {bucket = a} :: GetObjectRetention)

-- | The key name for the object whose retention settings you want to
-- retrieve.
getObjectRetention_key :: Lens.Lens' GetObjectRetention ObjectKey
getObjectRetention_key = Lens.lens (\GetObjectRetention' {key} -> key) (\s@GetObjectRetention' {} a -> s {key = a} :: GetObjectRetention)

instance Core.AWSRequest GetObjectRetention where
  type
    AWSResponse GetObjectRetention =
      GetObjectRetentionResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetObjectRetentionResponse'
            Core.<$> (Core.parseXML x)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetObjectRetention

instance Core.NFData GetObjectRetention

instance Core.ToHeaders GetObjectRetention where
  toHeaders GetObjectRetention' {..} =
    Core.mconcat
      [ "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner,
        "x-amz-request-payer" Core.=# requestPayer
      ]

instance Core.ToPath GetObjectRetention where
  toPath GetObjectRetention' {..} =
    Core.mconcat
      ["/", Core.toBS bucket, "/", Core.toBS key]

instance Core.ToQuery GetObjectRetention where
  toQuery GetObjectRetention' {..} =
    Core.mconcat
      ["versionId" Core.=: versionId, "retention"]

-- | /See:/ 'newGetObjectRetentionResponse' smart constructor.
data GetObjectRetentionResponse = GetObjectRetentionResponse'
  { -- | The container element for an object\'s retention settings.
    retention :: Core.Maybe ObjectLockRetention,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetObjectRetentionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'retention', 'getObjectRetentionResponse_retention' - The container element for an object\'s retention settings.
--
-- 'httpStatus', 'getObjectRetentionResponse_httpStatus' - The response's http status code.
newGetObjectRetentionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetObjectRetentionResponse
newGetObjectRetentionResponse pHttpStatus_ =
  GetObjectRetentionResponse'
    { retention =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The container element for an object\'s retention settings.
getObjectRetentionResponse_retention :: Lens.Lens' GetObjectRetentionResponse (Core.Maybe ObjectLockRetention)
getObjectRetentionResponse_retention = Lens.lens (\GetObjectRetentionResponse' {retention} -> retention) (\s@GetObjectRetentionResponse' {} a -> s {retention = a} :: GetObjectRetentionResponse)

-- | The response's http status code.
getObjectRetentionResponse_httpStatus :: Lens.Lens' GetObjectRetentionResponse Core.Int
getObjectRetentionResponse_httpStatus = Lens.lens (\GetObjectRetentionResponse' {httpStatus} -> httpStatus) (\s@GetObjectRetentionResponse' {} a -> s {httpStatus = a} :: GetObjectRetentionResponse)

instance Core.NFData GetObjectRetentionResponse
