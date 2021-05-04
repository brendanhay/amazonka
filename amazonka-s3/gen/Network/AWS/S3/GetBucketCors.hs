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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newGetBucketCors' smart constructor.
data GetBucketCors = GetBucketCors'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The bucket name for which to get the cors configuration.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { expectedBucketOwner =
        Prelude.Nothing,
      bucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
getBucketCors_expectedBucketOwner :: Lens.Lens' GetBucketCors (Prelude.Maybe Prelude.Text)
getBucketCors_expectedBucketOwner = Lens.lens (\GetBucketCors' {expectedBucketOwner} -> expectedBucketOwner) (\s@GetBucketCors' {} a -> s {expectedBucketOwner = a} :: GetBucketCors)

-- | The bucket name for which to get the cors configuration.
getBucketCors_bucket :: Lens.Lens' GetBucketCors BucketName
getBucketCors_bucket = Lens.lens (\GetBucketCors' {bucket} -> bucket) (\s@GetBucketCors' {} a -> s {bucket = a} :: GetBucketCors)

instance Prelude.AWSRequest GetBucketCors where
  type Rs GetBucketCors = GetBucketCorsResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetBucketCorsResponse'
            Prelude.<$> (Prelude.may (Prelude.parseXMLList "CORSRule") x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBucketCors

instance Prelude.NFData GetBucketCors

instance Prelude.ToHeaders GetBucketCors where
  toHeaders GetBucketCors' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Prelude.=# expectedBucketOwner
      ]

instance Prelude.ToPath GetBucketCors where
  toPath GetBucketCors' {..} =
    Prelude.mconcat ["/", Prelude.toBS bucket]

instance Prelude.ToQuery GetBucketCors where
  toQuery = Prelude.const (Prelude.mconcat ["cors"])

-- | /See:/ 'newGetBucketCorsResponse' smart constructor.
data GetBucketCorsResponse = GetBucketCorsResponse'
  { -- | A set of origins and methods (cross-origin access that you want to
    -- allow). You can add up to 100 rules to the configuration.
    cORSRules :: Prelude.Maybe [CORSRule],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetBucketCorsResponse
newGetBucketCorsResponse pHttpStatus_ =
  GetBucketCorsResponse'
    { cORSRules = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A set of origins and methods (cross-origin access that you want to
-- allow). You can add up to 100 rules to the configuration.
getBucketCorsResponse_cORSRules :: Lens.Lens' GetBucketCorsResponse (Prelude.Maybe [CORSRule])
getBucketCorsResponse_cORSRules = Lens.lens (\GetBucketCorsResponse' {cORSRules} -> cORSRules) (\s@GetBucketCorsResponse' {} a -> s {cORSRules = a} :: GetBucketCorsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
getBucketCorsResponse_httpStatus :: Lens.Lens' GetBucketCorsResponse Prelude.Int
getBucketCorsResponse_httpStatus = Lens.lens (\GetBucketCorsResponse' {httpStatus} -> httpStatus) (\s@GetBucketCorsResponse' {} a -> s {httpStatus = a} :: GetBucketCorsResponse)

instance Prelude.NFData GetBucketCorsResponse
