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
-- Module      : Amazonka.S3.GetBucketLocation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the Region the bucket resides in. You set the bucket\'s Region
-- using the @LocationConstraint@ request parameter in a @CreateBucket@
-- request. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket>.
--
-- To use this implementation of the operation, you must be the bucket
-- owner.
--
-- To use this API against an access point, provide the alias of the access
-- point in place of the bucket name.
--
-- The following operations are related to @GetBucketLocation@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket>
module Amazonka.S3.GetBucketLocation
  ( -- * Creating a Request
    GetBucketLocation (..),
    newGetBucketLocation,

    -- * Request Lenses
    getBucketLocation_expectedBucketOwner,
    getBucketLocation_bucket,

    -- * Destructuring the Response
    GetBucketLocationResponse (..),
    newGetBucketLocationResponse,

    -- * Response Lenses
    getBucketLocationResponse_httpStatus,
    getBucketLocationResponse_locationConstraint,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newGetBucketLocation' smart constructor.
data GetBucketLocation = GetBucketLocation'
  { -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the bucket for which to get the location.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBucketLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'getBucketLocation_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'bucket', 'getBucketLocation_bucket' - The name of the bucket for which to get the location.
newGetBucketLocation ::
  -- | 'bucket'
  BucketName ->
  GetBucketLocation
newGetBucketLocation pBucket_ =
  GetBucketLocation'
    { expectedBucketOwner =
        Prelude.Nothing,
      bucket = pBucket_
    }

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
getBucketLocation_expectedBucketOwner :: Lens.Lens' GetBucketLocation (Prelude.Maybe Prelude.Text)
getBucketLocation_expectedBucketOwner = Lens.lens (\GetBucketLocation' {expectedBucketOwner} -> expectedBucketOwner) (\s@GetBucketLocation' {} a -> s {expectedBucketOwner = a} :: GetBucketLocation)

-- | The name of the bucket for which to get the location.
getBucketLocation_bucket :: Lens.Lens' GetBucketLocation BucketName
getBucketLocation_bucket = Lens.lens (\GetBucketLocation' {bucket} -> bucket) (\s@GetBucketLocation' {} a -> s {bucket = a} :: GetBucketLocation)

instance Core.AWSRequest GetBucketLocation where
  type
    AWSResponse GetBucketLocation =
      GetBucketLocationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetBucketLocationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Data.parseXML x)
      )

instance Prelude.Hashable GetBucketLocation where
  hashWithSalt _salt GetBucketLocation' {..} =
    _salt
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` bucket

instance Prelude.NFData GetBucketLocation where
  rnf GetBucketLocation' {..} =
    Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf bucket

instance Data.ToHeaders GetBucketLocation where
  toHeaders GetBucketLocation' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner
      ]

instance Data.ToPath GetBucketLocation where
  toPath GetBucketLocation' {..} =
    Prelude.mconcat ["/", Data.toBS bucket]

instance Data.ToQuery GetBucketLocation where
  toQuery =
    Prelude.const (Prelude.mconcat ["location"])

-- | /See:/ 'newGetBucketLocationResponse' smart constructor.
data GetBucketLocationResponse = GetBucketLocationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Specifies the Region where the bucket resides. For a list of all the
    -- Amazon S3 supported location constraints by Region, see
    -- <https://docs.aws.amazon.com/general/latest/gr/rande.html#s3_region Regions and Endpoints>.
    -- Buckets in Region @us-east-1@ have a LocationConstraint of @null@.
    locationConstraint :: LocationConstraint
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBucketLocationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getBucketLocationResponse_httpStatus' - The response's http status code.
--
-- 'locationConstraint', 'getBucketLocationResponse_locationConstraint' - Specifies the Region where the bucket resides. For a list of all the
-- Amazon S3 supported location constraints by Region, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html#s3_region Regions and Endpoints>.
-- Buckets in Region @us-east-1@ have a LocationConstraint of @null@.
newGetBucketLocationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'locationConstraint'
  LocationConstraint ->
  GetBucketLocationResponse
newGetBucketLocationResponse
  pHttpStatus_
  pLocationConstraint_ =
    GetBucketLocationResponse'
      { httpStatus =
          pHttpStatus_,
        locationConstraint = pLocationConstraint_
      }

-- | The response's http status code.
getBucketLocationResponse_httpStatus :: Lens.Lens' GetBucketLocationResponse Prelude.Int
getBucketLocationResponse_httpStatus = Lens.lens (\GetBucketLocationResponse' {httpStatus} -> httpStatus) (\s@GetBucketLocationResponse' {} a -> s {httpStatus = a} :: GetBucketLocationResponse)

-- | Specifies the Region where the bucket resides. For a list of all the
-- Amazon S3 supported location constraints by Region, see
-- <https://docs.aws.amazon.com/general/latest/gr/rande.html#s3_region Regions and Endpoints>.
-- Buckets in Region @us-east-1@ have a LocationConstraint of @null@.
getBucketLocationResponse_locationConstraint :: Lens.Lens' GetBucketLocationResponse LocationConstraint
getBucketLocationResponse_locationConstraint = Lens.lens (\GetBucketLocationResponse' {locationConstraint} -> locationConstraint) (\s@GetBucketLocationResponse' {} a -> s {locationConstraint = a} :: GetBucketLocationResponse)

instance Prelude.NFData GetBucketLocationResponse where
  rnf GetBucketLocationResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf locationConstraint
