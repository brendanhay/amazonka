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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an object\'s current legal hold status. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Locking Objects>.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- The following action is related to @GetObjectLegalHold@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObjectAttributes.html GetObjectAttributes>
module Amazonka.S3.GetObjectLegalHold
  ( -- * Creating a Request
    GetObjectLegalHold (..),
    newGetObjectLegalHold,

    -- * Request Lenses
    getObjectLegalHold_expectedBucketOwner,
    getObjectLegalHold_requestPayer,
    getObjectLegalHold_versionId,
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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newGetObjectLegalHold' smart constructor.
data GetObjectLegalHold = GetObjectLegalHold'
  { -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    requestPayer :: Prelude.Maybe RequestPayer,
    -- | The version ID of the object whose legal hold status you want to
    -- retrieve.
    versionId :: Prelude.Maybe ObjectVersionId,
    -- | The bucket name containing the object whose legal hold status you want
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
    -- | The key name for the object whose legal hold status you want to
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
-- 'expectedBucketOwner', 'getObjectLegalHold_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'requestPayer', 'getObjectLegalHold_requestPayer' - Undocumented member.
--
-- 'versionId', 'getObjectLegalHold_versionId' - The version ID of the object whose legal hold status you want to
-- retrieve.
--
-- 'bucket', 'getObjectLegalHold_bucket' - The bucket name containing the object whose legal hold status you want
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
-- 'key', 'getObjectLegalHold_key' - The key name for the object whose legal hold status you want to
-- retrieve.
newGetObjectLegalHold ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  GetObjectLegalHold
newGetObjectLegalHold pBucket_ pKey_ =
  GetObjectLegalHold'
    { expectedBucketOwner =
        Prelude.Nothing,
      requestPayer = Prelude.Nothing,
      versionId = Prelude.Nothing,
      bucket = pBucket_,
      key = pKey_
    }

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
getObjectLegalHold_expectedBucketOwner :: Lens.Lens' GetObjectLegalHold (Prelude.Maybe Prelude.Text)
getObjectLegalHold_expectedBucketOwner = Lens.lens (\GetObjectLegalHold' {expectedBucketOwner} -> expectedBucketOwner) (\s@GetObjectLegalHold' {} a -> s {expectedBucketOwner = a} :: GetObjectLegalHold)

-- | Undocumented member.
getObjectLegalHold_requestPayer :: Lens.Lens' GetObjectLegalHold (Prelude.Maybe RequestPayer)
getObjectLegalHold_requestPayer = Lens.lens (\GetObjectLegalHold' {requestPayer} -> requestPayer) (\s@GetObjectLegalHold' {} a -> s {requestPayer = a} :: GetObjectLegalHold)

-- | The version ID of the object whose legal hold status you want to
-- retrieve.
getObjectLegalHold_versionId :: Lens.Lens' GetObjectLegalHold (Prelude.Maybe ObjectVersionId)
getObjectLegalHold_versionId = Lens.lens (\GetObjectLegalHold' {versionId} -> versionId) (\s@GetObjectLegalHold' {} a -> s {versionId = a} :: GetObjectLegalHold)

-- | The bucket name containing the object whose legal hold status you want
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

-- | The key name for the object whose legal hold status you want to
-- retrieve.
getObjectLegalHold_key :: Lens.Lens' GetObjectLegalHold ObjectKey
getObjectLegalHold_key = Lens.lens (\GetObjectLegalHold' {key} -> key) (\s@GetObjectLegalHold' {} a -> s {key = a} :: GetObjectLegalHold)

instance Core.AWSRequest GetObjectLegalHold where
  type
    AWSResponse GetObjectLegalHold =
      GetObjectLegalHoldResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetObjectLegalHoldResponse'
            Prelude.<$> (Data.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetObjectLegalHold where
  hashWithSalt _salt GetObjectLegalHold' {..} =
    _salt
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` requestPayer
      `Prelude.hashWithSalt` versionId
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` key

instance Prelude.NFData GetObjectLegalHold where
  rnf GetObjectLegalHold' {..} =
    Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf requestPayer
      `Prelude.seq` Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf key

instance Data.ToHeaders GetObjectLegalHold where
  toHeaders GetObjectLegalHold' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner,
        "x-amz-request-payer" Data.=# requestPayer
      ]

instance Data.ToPath GetObjectLegalHold where
  toPath GetObjectLegalHold' {..} =
    Prelude.mconcat
      ["/", Data.toBS bucket, "/", Data.toBS key]

instance Data.ToQuery GetObjectLegalHold where
  toQuery GetObjectLegalHold' {..} =
    Prelude.mconcat
      ["versionId" Data.=: versionId, "legal-hold"]

-- | /See:/ 'newGetObjectLegalHoldResponse' smart constructor.
data GetObjectLegalHoldResponse = GetObjectLegalHoldResponse'
  { -- | The current legal hold status for the specified object.
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
-- 'legalHold', 'getObjectLegalHoldResponse_legalHold' - The current legal hold status for the specified object.
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

-- | The current legal hold status for the specified object.
getObjectLegalHoldResponse_legalHold :: Lens.Lens' GetObjectLegalHoldResponse (Prelude.Maybe ObjectLockLegalHold)
getObjectLegalHoldResponse_legalHold = Lens.lens (\GetObjectLegalHoldResponse' {legalHold} -> legalHold) (\s@GetObjectLegalHoldResponse' {} a -> s {legalHold = a} :: GetObjectLegalHoldResponse)

-- | The response's http status code.
getObjectLegalHoldResponse_httpStatus :: Lens.Lens' GetObjectLegalHoldResponse Prelude.Int
getObjectLegalHoldResponse_httpStatus = Lens.lens (\GetObjectLegalHoldResponse' {httpStatus} -> httpStatus) (\s@GetObjectLegalHoldResponse' {} a -> s {httpStatus = a} :: GetObjectLegalHoldResponse)

instance Prelude.NFData GetObjectLegalHoldResponse where
  rnf GetObjectLegalHoldResponse' {..} =
    Prelude.rnf legalHold
      `Prelude.seq` Prelude.rnf httpStatus
