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
-- Module      : Amazonka.S3.GetObjectRetention
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves an object\'s retention settings. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Locking Objects>.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- The following action is related to @GetObjectRetention@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObjectAttributes.html GetObjectAttributes>
module Amazonka.S3.GetObjectRetention
  ( -- * Creating a Request
    GetObjectRetention (..),
    newGetObjectRetention,

    -- * Request Lenses
    getObjectRetention_expectedBucketOwner,
    getObjectRetention_requestPayer,
    getObjectRetention_versionId,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newGetObjectRetention' smart constructor.
data GetObjectRetention = GetObjectRetention'
  { -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    requestPayer :: Prelude.Maybe RequestPayer,
    -- | The version ID for the object whose retention settings you want to
    -- retrieve.
    versionId :: Prelude.Maybe ObjectVersionId,
    -- | The bucket name containing the object whose retention settings you want
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
    -- | The key name for the object whose retention settings you want to
    -- retrieve.
    key :: ObjectKey
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetObjectRetention' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'getObjectRetention_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'requestPayer', 'getObjectRetention_requestPayer' - Undocumented member.
--
-- 'versionId', 'getObjectRetention_versionId' - The version ID for the object whose retention settings you want to
-- retrieve.
--
-- 'bucket', 'getObjectRetention_bucket' - The bucket name containing the object whose retention settings you want
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
        Prelude.Nothing,
      requestPayer = Prelude.Nothing,
      versionId = Prelude.Nothing,
      bucket = pBucket_,
      key = pKey_
    }

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
getObjectRetention_expectedBucketOwner :: Lens.Lens' GetObjectRetention (Prelude.Maybe Prelude.Text)
getObjectRetention_expectedBucketOwner = Lens.lens (\GetObjectRetention' {expectedBucketOwner} -> expectedBucketOwner) (\s@GetObjectRetention' {} a -> s {expectedBucketOwner = a} :: GetObjectRetention)

-- | Undocumented member.
getObjectRetention_requestPayer :: Lens.Lens' GetObjectRetention (Prelude.Maybe RequestPayer)
getObjectRetention_requestPayer = Lens.lens (\GetObjectRetention' {requestPayer} -> requestPayer) (\s@GetObjectRetention' {} a -> s {requestPayer = a} :: GetObjectRetention)

-- | The version ID for the object whose retention settings you want to
-- retrieve.
getObjectRetention_versionId :: Lens.Lens' GetObjectRetention (Prelude.Maybe ObjectVersionId)
getObjectRetention_versionId = Lens.lens (\GetObjectRetention' {versionId} -> versionId) (\s@GetObjectRetention' {} a -> s {versionId = a} :: GetObjectRetention)

-- | The bucket name containing the object whose retention settings you want
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
  request overrides =
    Request.s3vhost
      Prelude.. Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetObjectRetentionResponse'
            Prelude.<$> (Data.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetObjectRetention where
  hashWithSalt _salt GetObjectRetention' {..} =
    _salt
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` requestPayer
      `Prelude.hashWithSalt` versionId
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` key

instance Prelude.NFData GetObjectRetention where
  rnf GetObjectRetention' {..} =
    Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf requestPayer
      `Prelude.seq` Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf key

instance Data.ToHeaders GetObjectRetention where
  toHeaders GetObjectRetention' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner,
        "x-amz-request-payer" Data.=# requestPayer
      ]

instance Data.ToPath GetObjectRetention where
  toPath GetObjectRetention' {..} =
    Prelude.mconcat
      ["/", Data.toBS bucket, "/", Data.toBS key]

instance Data.ToQuery GetObjectRetention where
  toQuery GetObjectRetention' {..} =
    Prelude.mconcat
      ["versionId" Data.=: versionId, "retention"]

-- | /See:/ 'newGetObjectRetentionResponse' smart constructor.
data GetObjectRetentionResponse = GetObjectRetentionResponse'
  { -- | The container element for an object\'s retention settings.
    retention :: Prelude.Maybe ObjectLockRetention,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetObjectRetentionResponse
newGetObjectRetentionResponse pHttpStatus_ =
  GetObjectRetentionResponse'
    { retention =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The container element for an object\'s retention settings.
getObjectRetentionResponse_retention :: Lens.Lens' GetObjectRetentionResponse (Prelude.Maybe ObjectLockRetention)
getObjectRetentionResponse_retention = Lens.lens (\GetObjectRetentionResponse' {retention} -> retention) (\s@GetObjectRetentionResponse' {} a -> s {retention = a} :: GetObjectRetentionResponse)

-- | The response's http status code.
getObjectRetentionResponse_httpStatus :: Lens.Lens' GetObjectRetentionResponse Prelude.Int
getObjectRetentionResponse_httpStatus = Lens.lens (\GetObjectRetentionResponse' {httpStatus} -> httpStatus) (\s@GetObjectRetentionResponse' {} a -> s {httpStatus = a} :: GetObjectRetentionResponse)

instance Prelude.NFData GetObjectRetentionResponse where
  rnf GetObjectRetentionResponse' {..} =
    Prelude.rnf retention
      `Prelude.seq` Prelude.rnf httpStatus
