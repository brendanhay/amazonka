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
-- Module      : Network.AWS.S3.GetBucketVersioning
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the versioning state of a bucket.
--
-- To retrieve the versioning state of a bucket, you must be the bucket
-- owner.
--
-- This implementation also returns the MFA Delete status of the versioning
-- state. If the MFA Delete status is @enabled@, the bucket owner must use
-- an authentication device to change the versioning state of the bucket.
--
-- The following operations are related to @GetBucketVersioning@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutObject.html PutObject>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteObject.html DeleteObject>
module Network.AWS.S3.GetBucketVersioning
  ( -- * Creating a Request
    GetBucketVersioning (..),
    newGetBucketVersioning,

    -- * Request Lenses
    getBucketVersioning_expectedBucketOwner,
    getBucketVersioning_bucket,

    -- * Destructuring the Response
    GetBucketVersioningResponse (..),
    newGetBucketVersioningResponse,

    -- * Response Lenses
    getBucketVersioningResponse_status,
    getBucketVersioningResponse_mfaDelete,
    getBucketVersioningResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newGetBucketVersioning' smart constructor.
data GetBucketVersioning = GetBucketVersioning'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Core.Text,
    -- | The name of the bucket for which to get the versioning information.
    bucket :: BucketName
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetBucketVersioning' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'getBucketVersioning_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'bucket', 'getBucketVersioning_bucket' - The name of the bucket for which to get the versioning information.
newGetBucketVersioning ::
  -- | 'bucket'
  BucketName ->
  GetBucketVersioning
newGetBucketVersioning pBucket_ =
  GetBucketVersioning'
    { expectedBucketOwner =
        Core.Nothing,
      bucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
getBucketVersioning_expectedBucketOwner :: Lens.Lens' GetBucketVersioning (Core.Maybe Core.Text)
getBucketVersioning_expectedBucketOwner = Lens.lens (\GetBucketVersioning' {expectedBucketOwner} -> expectedBucketOwner) (\s@GetBucketVersioning' {} a -> s {expectedBucketOwner = a} :: GetBucketVersioning)

-- | The name of the bucket for which to get the versioning information.
getBucketVersioning_bucket :: Lens.Lens' GetBucketVersioning BucketName
getBucketVersioning_bucket = Lens.lens (\GetBucketVersioning' {bucket} -> bucket) (\s@GetBucketVersioning' {} a -> s {bucket = a} :: GetBucketVersioning)

instance Core.AWSRequest GetBucketVersioning where
  type
    AWSResponse GetBucketVersioning =
      GetBucketVersioningResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetBucketVersioningResponse'
            Core.<$> (x Core..@? "Status")
            Core.<*> (x Core..@? "MfaDelete")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetBucketVersioning

instance Core.NFData GetBucketVersioning

instance Core.ToHeaders GetBucketVersioning where
  toHeaders GetBucketVersioning' {..} =
    Core.mconcat
      [ "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner
      ]

instance Core.ToPath GetBucketVersioning where
  toPath GetBucketVersioning' {..} =
    Core.mconcat ["/", Core.toBS bucket]

instance Core.ToQuery GetBucketVersioning where
  toQuery = Core.const (Core.mconcat ["versioning"])

-- | /See:/ 'newGetBucketVersioningResponse' smart constructor.
data GetBucketVersioningResponse = GetBucketVersioningResponse'
  { -- | The versioning state of the bucket.
    status :: Core.Maybe BucketVersioningStatus,
    -- | Specifies whether MFA delete is enabled in the bucket versioning
    -- configuration. This element is only returned if the bucket has been
    -- configured with MFA delete. If the bucket has never been so configured,
    -- this element is not returned.
    mfaDelete :: Core.Maybe MFADeleteStatus,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetBucketVersioningResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'getBucketVersioningResponse_status' - The versioning state of the bucket.
--
-- 'mfaDelete', 'getBucketVersioningResponse_mfaDelete' - Specifies whether MFA delete is enabled in the bucket versioning
-- configuration. This element is only returned if the bucket has been
-- configured with MFA delete. If the bucket has never been so configured,
-- this element is not returned.
--
-- 'httpStatus', 'getBucketVersioningResponse_httpStatus' - The response's http status code.
newGetBucketVersioningResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetBucketVersioningResponse
newGetBucketVersioningResponse pHttpStatus_ =
  GetBucketVersioningResponse'
    { status = Core.Nothing,
      mfaDelete = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The versioning state of the bucket.
getBucketVersioningResponse_status :: Lens.Lens' GetBucketVersioningResponse (Core.Maybe BucketVersioningStatus)
getBucketVersioningResponse_status = Lens.lens (\GetBucketVersioningResponse' {status} -> status) (\s@GetBucketVersioningResponse' {} a -> s {status = a} :: GetBucketVersioningResponse)

-- | Specifies whether MFA delete is enabled in the bucket versioning
-- configuration. This element is only returned if the bucket has been
-- configured with MFA delete. If the bucket has never been so configured,
-- this element is not returned.
getBucketVersioningResponse_mfaDelete :: Lens.Lens' GetBucketVersioningResponse (Core.Maybe MFADeleteStatus)
getBucketVersioningResponse_mfaDelete = Lens.lens (\GetBucketVersioningResponse' {mfaDelete} -> mfaDelete) (\s@GetBucketVersioningResponse' {} a -> s {mfaDelete = a} :: GetBucketVersioningResponse)

-- | The response's http status code.
getBucketVersioningResponse_httpStatus :: Lens.Lens' GetBucketVersioningResponse Core.Int
getBucketVersioningResponse_httpStatus = Lens.lens (\GetBucketVersioningResponse' {httpStatus} -> httpStatus) (\s@GetBucketVersioningResponse' {} a -> s {httpStatus = a} :: GetBucketVersioningResponse)

instance Core.NFData GetBucketVersioningResponse
