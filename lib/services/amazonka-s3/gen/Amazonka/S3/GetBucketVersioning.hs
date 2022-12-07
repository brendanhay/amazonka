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
-- Module      : Amazonka.S3.GetBucketVersioning
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.S3.GetBucketVersioning
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
    getBucketVersioningResponse_mfaDelete,
    getBucketVersioningResponse_status,
    getBucketVersioningResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newGetBucketVersioning' smart constructor.
data GetBucketVersioning = GetBucketVersioning'
  { -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the bucket for which to get the versioning information.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBucketVersioning' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'getBucketVersioning_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'bucket', 'getBucketVersioning_bucket' - The name of the bucket for which to get the versioning information.
newGetBucketVersioning ::
  -- | 'bucket'
  BucketName ->
  GetBucketVersioning
newGetBucketVersioning pBucket_ =
  GetBucketVersioning'
    { expectedBucketOwner =
        Prelude.Nothing,
      bucket = pBucket_
    }

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
getBucketVersioning_expectedBucketOwner :: Lens.Lens' GetBucketVersioning (Prelude.Maybe Prelude.Text)
getBucketVersioning_expectedBucketOwner = Lens.lens (\GetBucketVersioning' {expectedBucketOwner} -> expectedBucketOwner) (\s@GetBucketVersioning' {} a -> s {expectedBucketOwner = a} :: GetBucketVersioning)

-- | The name of the bucket for which to get the versioning information.
getBucketVersioning_bucket :: Lens.Lens' GetBucketVersioning BucketName
getBucketVersioning_bucket = Lens.lens (\GetBucketVersioning' {bucket} -> bucket) (\s@GetBucketVersioning' {} a -> s {bucket = a} :: GetBucketVersioning)

instance Core.AWSRequest GetBucketVersioning where
  type
    AWSResponse GetBucketVersioning =
      GetBucketVersioningResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetBucketVersioningResponse'
            Prelude.<$> (x Data..@? "MfaDelete")
            Prelude.<*> (x Data..@? "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBucketVersioning where
  hashWithSalt _salt GetBucketVersioning' {..} =
    _salt `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` bucket

instance Prelude.NFData GetBucketVersioning where
  rnf GetBucketVersioning' {..} =
    Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf bucket

instance Data.ToHeaders GetBucketVersioning where
  toHeaders GetBucketVersioning' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner
      ]

instance Data.ToPath GetBucketVersioning where
  toPath GetBucketVersioning' {..} =
    Prelude.mconcat ["/", Data.toBS bucket]

instance Data.ToQuery GetBucketVersioning where
  toQuery =
    Prelude.const (Prelude.mconcat ["versioning"])

-- | /See:/ 'newGetBucketVersioningResponse' smart constructor.
data GetBucketVersioningResponse = GetBucketVersioningResponse'
  { -- | Specifies whether MFA delete is enabled in the bucket versioning
    -- configuration. This element is only returned if the bucket has been
    -- configured with MFA delete. If the bucket has never been so configured,
    -- this element is not returned.
    mfaDelete :: Prelude.Maybe MFADeleteStatus,
    -- | The versioning state of the bucket.
    status :: Prelude.Maybe BucketVersioningStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBucketVersioningResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mfaDelete', 'getBucketVersioningResponse_mfaDelete' - Specifies whether MFA delete is enabled in the bucket versioning
-- configuration. This element is only returned if the bucket has been
-- configured with MFA delete. If the bucket has never been so configured,
-- this element is not returned.
--
-- 'status', 'getBucketVersioningResponse_status' - The versioning state of the bucket.
--
-- 'httpStatus', 'getBucketVersioningResponse_httpStatus' - The response's http status code.
newGetBucketVersioningResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBucketVersioningResponse
newGetBucketVersioningResponse pHttpStatus_ =
  GetBucketVersioningResponse'
    { mfaDelete =
        Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Specifies whether MFA delete is enabled in the bucket versioning
-- configuration. This element is only returned if the bucket has been
-- configured with MFA delete. If the bucket has never been so configured,
-- this element is not returned.
getBucketVersioningResponse_mfaDelete :: Lens.Lens' GetBucketVersioningResponse (Prelude.Maybe MFADeleteStatus)
getBucketVersioningResponse_mfaDelete = Lens.lens (\GetBucketVersioningResponse' {mfaDelete} -> mfaDelete) (\s@GetBucketVersioningResponse' {} a -> s {mfaDelete = a} :: GetBucketVersioningResponse)

-- | The versioning state of the bucket.
getBucketVersioningResponse_status :: Lens.Lens' GetBucketVersioningResponse (Prelude.Maybe BucketVersioningStatus)
getBucketVersioningResponse_status = Lens.lens (\GetBucketVersioningResponse' {status} -> status) (\s@GetBucketVersioningResponse' {} a -> s {status = a} :: GetBucketVersioningResponse)

-- | The response's http status code.
getBucketVersioningResponse_httpStatus :: Lens.Lens' GetBucketVersioningResponse Prelude.Int
getBucketVersioningResponse_httpStatus = Lens.lens (\GetBucketVersioningResponse' {httpStatus} -> httpStatus) (\s@GetBucketVersioningResponse' {} a -> s {httpStatus = a} :: GetBucketVersioningResponse)

instance Prelude.NFData GetBucketVersioningResponse where
  rnf GetBucketVersioningResponse' {..} =
    Prelude.rnf mfaDelete
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
