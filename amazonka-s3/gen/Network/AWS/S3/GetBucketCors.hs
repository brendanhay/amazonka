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
-- Module      : Network.AWS.S3.GetBucketCors
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the cors configuration information set for the bucket.
--
-- To use this operation, you must have permission to perform the
-- s3:GetBucketCORS action. By default, the bucket owner has this
-- permission and can grant it to others.
--
-- For more information about cors, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/cors.html Enabling Cross-Origin Resource Sharing>.
--
-- The following operations are related to @GetBucketCors@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketCors.html PutBucketCors>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketCors.html DeleteBucketCors>
module Network.AWS.S3.GetBucketCors
  ( -- * Creating a Request
    GetBucketCors (..),
    newGetBucketCors,

    -- * Request Lenses
    getBucketCors_expectedBucketOwner,
    getBucketCors_bucket,

    -- * Destructuring the Response
    GetBucketCorsResponse (..),
    newGetBucketCorsResponse,

    -- * Response Lenses
    getBucketCorsResponse_cORSRules,
    getBucketCorsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newGetBucketCors' smart constructor.
data GetBucketCors = GetBucketCors'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Core.Text,
    -- | The bucket name for which to get the cors configuration.
    bucket :: BucketName
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetBucketCors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'getBucketCors_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'bucket', 'getBucketCors_bucket' - The bucket name for which to get the cors configuration.
newGetBucketCors ::
  -- | 'bucket'
  BucketName ->
  GetBucketCors
newGetBucketCors pBucket_ =
  GetBucketCors'
    { expectedBucketOwner = Core.Nothing,
      bucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
getBucketCors_expectedBucketOwner :: Lens.Lens' GetBucketCors (Core.Maybe Core.Text)
getBucketCors_expectedBucketOwner = Lens.lens (\GetBucketCors' {expectedBucketOwner} -> expectedBucketOwner) (\s@GetBucketCors' {} a -> s {expectedBucketOwner = a} :: GetBucketCors)

-- | The bucket name for which to get the cors configuration.
getBucketCors_bucket :: Lens.Lens' GetBucketCors BucketName
getBucketCors_bucket = Lens.lens (\GetBucketCors' {bucket} -> bucket) (\s@GetBucketCors' {} a -> s {bucket = a} :: GetBucketCors)

instance Core.AWSRequest GetBucketCors where
  type
    AWSResponse GetBucketCors =
      GetBucketCorsResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetBucketCorsResponse'
            Core.<$> (Core.may (Core.parseXMLList "CORSRule") x)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetBucketCors

instance Core.NFData GetBucketCors

instance Core.ToHeaders GetBucketCors where
  toHeaders GetBucketCors' {..} =
    Core.mconcat
      [ "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner
      ]

instance Core.ToPath GetBucketCors where
  toPath GetBucketCors' {..} =
    Core.mconcat ["/", Core.toBS bucket]

instance Core.ToQuery GetBucketCors where
  toQuery = Core.const (Core.mconcat ["cors"])

-- | /See:/ 'newGetBucketCorsResponse' smart constructor.
data GetBucketCorsResponse = GetBucketCorsResponse'
  { -- | A set of origins and methods (cross-origin access that you want to
    -- allow). You can add up to 100 rules to the configuration.
    cORSRules :: Core.Maybe [CORSRule],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetBucketCorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cORSRules', 'getBucketCorsResponse_cORSRules' - A set of origins and methods (cross-origin access that you want to
-- allow). You can add up to 100 rules to the configuration.
--
-- 'httpStatus', 'getBucketCorsResponse_httpStatus' - The response's http status code.
newGetBucketCorsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetBucketCorsResponse
newGetBucketCorsResponse pHttpStatus_ =
  GetBucketCorsResponse'
    { cORSRules = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A set of origins and methods (cross-origin access that you want to
-- allow). You can add up to 100 rules to the configuration.
getBucketCorsResponse_cORSRules :: Lens.Lens' GetBucketCorsResponse (Core.Maybe [CORSRule])
getBucketCorsResponse_cORSRules = Lens.lens (\GetBucketCorsResponse' {cORSRules} -> cORSRules) (\s@GetBucketCorsResponse' {} a -> s {cORSRules = a} :: GetBucketCorsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getBucketCorsResponse_httpStatus :: Lens.Lens' GetBucketCorsResponse Core.Int
getBucketCorsResponse_httpStatus = Lens.lens (\GetBucketCorsResponse' {httpStatus} -> httpStatus) (\s@GetBucketCorsResponse' {} a -> s {httpStatus = a} :: GetBucketCorsResponse)

instance Core.NFData GetBucketCorsResponse
