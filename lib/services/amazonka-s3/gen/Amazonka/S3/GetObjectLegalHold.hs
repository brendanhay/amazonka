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
-- Module      : Amazonka.S3.GetObjectLegalHold
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an object\'s current Legal Hold status. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Locking Objects>.
--
-- This action is not supported by Amazon S3 on Outposts.
module Amazonka.S3.GetObjectLegalHold
  ( -- * Creating a Request
    GetObjectLegalHold (..),
    newGetObjectLegalHold,

    -- * Request Lenses
    getObjectLegalHold_versionId,
    getObjectLegalHold_requestPayer,
    getObjectLegalHold_expectedBucketOwner,
    getObjectLegalHold_bucket,
    getObjectLegalHold_key,

    -- * Destructuring the Response
    GetObjectLegalHoldResponse (..),
    newGetObjectLegalHoldResponse,

    -- * Response Lenses
    getObjectLegalHoldResponse_legalHold,
    getObjectLegalHoldResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newGetObjectLegalHold' smart constructor.
data GetObjectLegalHold = GetObjectLegalHold'
  { -- | The version ID of the object whose Legal Hold status you want to
    -- retrieve.
    versionId :: Prelude.Maybe ObjectVersionId,
    requestPayer :: Prelude.Maybe RequestPayer,
    -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The bucket name containing the object whose Legal Hold status you want
    -- to retrieve.
    --
    -- When using this action with an access point, you must direct requests to
    -- the access point hostname. The access point hostname takes the form
    -- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
    -- When using this action with an access point through the Amazon Web
    -- Services SDKs, you provide the access point ARN in place of the bucket
    -- name. For more information about access point ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-access-points.html Using access points>
    -- in the /Amazon S3 User Guide/.
    bucket :: BucketName,
    -- | The key name for the object whose Legal Hold status you want to
    -- retrieve.
    key :: ObjectKey
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetObjectLegalHold' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versionId', 'getObjectLegalHold_versionId' - The version ID of the object whose Legal Hold status you want to
-- retrieve.
--
-- 'requestPayer', 'getObjectLegalHold_requestPayer' - Undocumented member.
--
-- 'expectedBucketOwner', 'getObjectLegalHold_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'bucket', 'getObjectLegalHold_bucket' - The bucket name containing the object whose Legal Hold status you want
-- to retrieve.
--
-- When using this action with an access point, you must direct requests to
-- the access point hostname. The access point hostname takes the form
-- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
-- When using this action with an access point through the Amazon Web
-- Services SDKs, you provide the access point ARN in place of the bucket
-- name. For more information about access point ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-access-points.html Using access points>
-- in the /Amazon S3 User Guide/.
--
-- 'key', 'getObjectLegalHold_key' - The key name for the object whose Legal Hold status you want to
-- retrieve.
newGetObjectLegalHold ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  GetObjectLegalHold
newGetObjectLegalHold pBucket_ pKey_ =
  GetObjectLegalHold'
    { versionId = Prelude.Nothing,
      requestPayer = Prelude.Nothing,
      expectedBucketOwner = Prelude.Nothing,
      bucket = pBucket_,
      key = pKey_
    }

-- | The version ID of the object whose Legal Hold status you want to
-- retrieve.
getObjectLegalHold_versionId :: Lens.Lens' GetObjectLegalHold (Prelude.Maybe ObjectVersionId)
getObjectLegalHold_versionId = Lens.lens (\GetObjectLegalHold' {versionId} -> versionId) (\s@GetObjectLegalHold' {} a -> s {versionId = a} :: GetObjectLegalHold)

-- | Undocumented member.
getObjectLegalHold_requestPayer :: Lens.Lens' GetObjectLegalHold (Prelude.Maybe RequestPayer)
getObjectLegalHold_requestPayer = Lens.lens (\GetObjectLegalHold' {requestPayer} -> requestPayer) (\s@GetObjectLegalHold' {} a -> s {requestPayer = a} :: GetObjectLegalHold)

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
getObjectLegalHold_expectedBucketOwner :: Lens.Lens' GetObjectLegalHold (Prelude.Maybe Prelude.Text)
getObjectLegalHold_expectedBucketOwner = Lens.lens (\GetObjectLegalHold' {expectedBucketOwner} -> expectedBucketOwner) (\s@GetObjectLegalHold' {} a -> s {expectedBucketOwner = a} :: GetObjectLegalHold)

-- | The bucket name containing the object whose Legal Hold status you want
-- to retrieve.
--
-- When using this action with an access point, you must direct requests to
-- the access point hostname. The access point hostname takes the form
-- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
-- When using this action with an access point through the Amazon Web
-- Services SDKs, you provide the access point ARN in place of the bucket
-- name. For more information about access point ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-access-points.html Using access points>
-- in the /Amazon S3 User Guide/.
getObjectLegalHold_bucket :: Lens.Lens' GetObjectLegalHold BucketName
getObjectLegalHold_bucket = Lens.lens (\GetObjectLegalHold' {bucket} -> bucket) (\s@GetObjectLegalHold' {} a -> s {bucket = a} :: GetObjectLegalHold)

-- | The key name for the object whose Legal Hold status you want to
-- retrieve.
getObjectLegalHold_key :: Lens.Lens' GetObjectLegalHold ObjectKey
getObjectLegalHold_key = Lens.lens (\GetObjectLegalHold' {key} -> key) (\s@GetObjectLegalHold' {} a -> s {key = a} :: GetObjectLegalHold)

instance Core.AWSRequest GetObjectLegalHold where
  type
    AWSResponse GetObjectLegalHold =
      GetObjectLegalHoldResponse
  request =
    Request.s3vhost
      Prelude.. Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetObjectLegalHoldResponse'
            Prelude.<$> (Core.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetObjectLegalHold where
  hashWithSalt _salt GetObjectLegalHold' {..} =
    _salt `Prelude.hashWithSalt` versionId
      `Prelude.hashWithSalt` requestPayer
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` key

instance Prelude.NFData GetObjectLegalHold where
  rnf GetObjectLegalHold' {..} =
    Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf requestPayer
      `Prelude.seq` Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf key

instance Core.ToHeaders GetObjectLegalHold where
  toHeaders GetObjectLegalHold' {..} =
    Prelude.mconcat
      [ "x-amz-request-payer" Core.=# requestPayer,
        "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner
      ]

instance Core.ToPath GetObjectLegalHold where
  toPath GetObjectLegalHold' {..} =
    Prelude.mconcat
      ["/", Core.toBS bucket, "/", Core.toBS key]

instance Core.ToQuery GetObjectLegalHold where
  toQuery GetObjectLegalHold' {..} =
    Prelude.mconcat
      ["versionId" Core.=: versionId, "legal-hold"]

-- | /See:/ 'newGetObjectLegalHoldResponse' smart constructor.
data GetObjectLegalHoldResponse = GetObjectLegalHoldResponse'
  { -- | The current Legal Hold status for the specified object.
    legalHold :: Prelude.Maybe ObjectLockLegalHold,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetObjectLegalHoldResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'legalHold', 'getObjectLegalHoldResponse_legalHold' - The current Legal Hold status for the specified object.
--
-- 'httpStatus', 'getObjectLegalHoldResponse_httpStatus' - The response's http status code.
newGetObjectLegalHoldResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetObjectLegalHoldResponse
newGetObjectLegalHoldResponse pHttpStatus_ =
  GetObjectLegalHoldResponse'
    { legalHold =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current Legal Hold status for the specified object.
getObjectLegalHoldResponse_legalHold :: Lens.Lens' GetObjectLegalHoldResponse (Prelude.Maybe ObjectLockLegalHold)
getObjectLegalHoldResponse_legalHold = Lens.lens (\GetObjectLegalHoldResponse' {legalHold} -> legalHold) (\s@GetObjectLegalHoldResponse' {} a -> s {legalHold = a} :: GetObjectLegalHoldResponse)

-- | The response's http status code.
getObjectLegalHoldResponse_httpStatus :: Lens.Lens' GetObjectLegalHoldResponse Prelude.Int
getObjectLegalHoldResponse_httpStatus = Lens.lens (\GetObjectLegalHoldResponse' {httpStatus} -> httpStatus) (\s@GetObjectLegalHoldResponse' {} a -> s {httpStatus = a} :: GetObjectLegalHoldResponse)

instance Prelude.NFData GetObjectLegalHoldResponse where
  rnf GetObjectLegalHoldResponse' {..} =
    Prelude.rnf legalHold
      `Prelude.seq` Prelude.rnf httpStatus
