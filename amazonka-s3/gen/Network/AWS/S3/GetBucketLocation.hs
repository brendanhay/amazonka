{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.S3.GetBucketLocation
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- The following operations are related to @GetBucketLocation@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObject.html GetObject>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket>
module Network.AWS.S3.GetBucketLocation
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newGetBucketLocation' smart constructor.
data GetBucketLocation = GetBucketLocation'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the bucket for which to get the location.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetBucketLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'getBucketLocation_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
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

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
getBucketLocation_expectedBucketOwner :: Lens.Lens' GetBucketLocation (Prelude.Maybe Prelude.Text)
getBucketLocation_expectedBucketOwner = Lens.lens (\GetBucketLocation' {expectedBucketOwner} -> expectedBucketOwner) (\s@GetBucketLocation' {} a -> s {expectedBucketOwner = a} :: GetBucketLocation)

-- | The name of the bucket for which to get the location.
getBucketLocation_bucket :: Lens.Lens' GetBucketLocation BucketName
getBucketLocation_bucket = Lens.lens (\GetBucketLocation' {bucket} -> bucket) (\s@GetBucketLocation' {} a -> s {bucket = a} :: GetBucketLocation)

instance Prelude.AWSRequest GetBucketLocation where
  type Rs GetBucketLocation = GetBucketLocationResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetBucketLocationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Prelude.parseXML x)
      )

instance Prelude.Hashable GetBucketLocation

instance Prelude.NFData GetBucketLocation

instance Prelude.ToHeaders GetBucketLocation where
  toHeaders GetBucketLocation' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Prelude.=# expectedBucketOwner
      ]

instance Prelude.ToPath GetBucketLocation where
  toPath GetBucketLocation' {..} =
    Prelude.mconcat ["/", Prelude.toBS bucket]

instance Prelude.ToQuery GetBucketLocation where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData GetBucketLocationResponse
