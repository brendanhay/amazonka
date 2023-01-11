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
-- Module      : Amazonka.S3.GetBucketOwnershipControls
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves @OwnershipControls@ for an Amazon S3 bucket. To use this
-- operation, you must have the @s3:GetBucketOwnershipControls@ permission.
-- For more information about Amazon S3 permissions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-with-s3-actions.html Specifying permissions in a policy>.
--
-- For information about Amazon S3 Object Ownership, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/about-object-ownership.html Using Object Ownership>.
--
-- The following operations are related to @GetBucketOwnershipControls@:
--
-- -   PutBucketOwnershipControls
--
-- -   DeleteBucketOwnershipControls
module Amazonka.S3.GetBucketOwnershipControls
  ( -- * Creating a Request
    GetBucketOwnershipControls (..),
    newGetBucketOwnershipControls,

    -- * Request Lenses
    getBucketOwnershipControls_expectedBucketOwner,
    getBucketOwnershipControls_bucket,

    -- * Destructuring the Response
    GetBucketOwnershipControlsResponse (..),
    newGetBucketOwnershipControlsResponse,

    -- * Response Lenses
    getBucketOwnershipControlsResponse_ownershipControls,
    getBucketOwnershipControlsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newGetBucketOwnershipControls' smart constructor.
data GetBucketOwnershipControls = GetBucketOwnershipControls'
  { -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon S3 bucket whose @OwnershipControls@ you want to
    -- retrieve.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBucketOwnershipControls' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'getBucketOwnershipControls_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'bucket', 'getBucketOwnershipControls_bucket' - The name of the Amazon S3 bucket whose @OwnershipControls@ you want to
-- retrieve.
newGetBucketOwnershipControls ::
  -- | 'bucket'
  BucketName ->
  GetBucketOwnershipControls
newGetBucketOwnershipControls pBucket_ =
  GetBucketOwnershipControls'
    { expectedBucketOwner =
        Prelude.Nothing,
      bucket = pBucket_
    }

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
getBucketOwnershipControls_expectedBucketOwner :: Lens.Lens' GetBucketOwnershipControls (Prelude.Maybe Prelude.Text)
getBucketOwnershipControls_expectedBucketOwner = Lens.lens (\GetBucketOwnershipControls' {expectedBucketOwner} -> expectedBucketOwner) (\s@GetBucketOwnershipControls' {} a -> s {expectedBucketOwner = a} :: GetBucketOwnershipControls)

-- | The name of the Amazon S3 bucket whose @OwnershipControls@ you want to
-- retrieve.
getBucketOwnershipControls_bucket :: Lens.Lens' GetBucketOwnershipControls BucketName
getBucketOwnershipControls_bucket = Lens.lens (\GetBucketOwnershipControls' {bucket} -> bucket) (\s@GetBucketOwnershipControls' {} a -> s {bucket = a} :: GetBucketOwnershipControls)

instance Core.AWSRequest GetBucketOwnershipControls where
  type
    AWSResponse GetBucketOwnershipControls =
      GetBucketOwnershipControlsResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetBucketOwnershipControlsResponse'
            Prelude.<$> (Data.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBucketOwnershipControls where
  hashWithSalt _salt GetBucketOwnershipControls' {..} =
    _salt `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` bucket

instance Prelude.NFData GetBucketOwnershipControls where
  rnf GetBucketOwnershipControls' {..} =
    Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf bucket

instance Data.ToHeaders GetBucketOwnershipControls where
  toHeaders GetBucketOwnershipControls' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner
      ]

instance Data.ToPath GetBucketOwnershipControls where
  toPath GetBucketOwnershipControls' {..} =
    Prelude.mconcat ["/", Data.toBS bucket]

instance Data.ToQuery GetBucketOwnershipControls where
  toQuery =
    Prelude.const
      (Prelude.mconcat ["ownershipControls"])

-- | /See:/ 'newGetBucketOwnershipControlsResponse' smart constructor.
data GetBucketOwnershipControlsResponse = GetBucketOwnershipControlsResponse'
  { -- | The @OwnershipControls@ (BucketOwnerEnforced, BucketOwnerPreferred, or
    -- ObjectWriter) currently in effect for this Amazon S3 bucket.
    ownershipControls :: Prelude.Maybe OwnershipControls,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBucketOwnershipControlsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownershipControls', 'getBucketOwnershipControlsResponse_ownershipControls' - The @OwnershipControls@ (BucketOwnerEnforced, BucketOwnerPreferred, or
-- ObjectWriter) currently in effect for this Amazon S3 bucket.
--
-- 'httpStatus', 'getBucketOwnershipControlsResponse_httpStatus' - The response's http status code.
newGetBucketOwnershipControlsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBucketOwnershipControlsResponse
newGetBucketOwnershipControlsResponse pHttpStatus_ =
  GetBucketOwnershipControlsResponse'
    { ownershipControls =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @OwnershipControls@ (BucketOwnerEnforced, BucketOwnerPreferred, or
-- ObjectWriter) currently in effect for this Amazon S3 bucket.
getBucketOwnershipControlsResponse_ownershipControls :: Lens.Lens' GetBucketOwnershipControlsResponse (Prelude.Maybe OwnershipControls)
getBucketOwnershipControlsResponse_ownershipControls = Lens.lens (\GetBucketOwnershipControlsResponse' {ownershipControls} -> ownershipControls) (\s@GetBucketOwnershipControlsResponse' {} a -> s {ownershipControls = a} :: GetBucketOwnershipControlsResponse)

-- | The response's http status code.
getBucketOwnershipControlsResponse_httpStatus :: Lens.Lens' GetBucketOwnershipControlsResponse Prelude.Int
getBucketOwnershipControlsResponse_httpStatus = Lens.lens (\GetBucketOwnershipControlsResponse' {httpStatus} -> httpStatus) (\s@GetBucketOwnershipControlsResponse' {} a -> s {httpStatus = a} :: GetBucketOwnershipControlsResponse)

instance
  Prelude.NFData
    GetBucketOwnershipControlsResponse
  where
  rnf GetBucketOwnershipControlsResponse' {..} =
    Prelude.rnf ownershipControls
      `Prelude.seq` Prelude.rnf httpStatus
