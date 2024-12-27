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
-- Module      : Amazonka.S3.GetBucketCors
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the Cross-Origin Resource Sharing (CORS) configuration
-- information set for the bucket.
--
-- To use this operation, you must have permission to perform the
-- @s3:GetBucketCORS@ action. By default, the bucket owner has this
-- permission and can grant it to others.
--
-- For more information about CORS, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/cors.html Enabling Cross-Origin Resource Sharing>.
--
-- The following operations are related to @GetBucketCors@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketCors.html PutBucketCors>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketCors.html DeleteBucketCors>
module Amazonka.S3.GetBucketCors
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newGetBucketCors' smart constructor.
data GetBucketCors = GetBucketCors'
  { -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The bucket name for which to get the cors configuration.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBucketCors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'getBucketCors_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
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

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
getBucketCors_expectedBucketOwner :: Lens.Lens' GetBucketCors (Prelude.Maybe Prelude.Text)
getBucketCors_expectedBucketOwner = Lens.lens (\GetBucketCors' {expectedBucketOwner} -> expectedBucketOwner) (\s@GetBucketCors' {} a -> s {expectedBucketOwner = a} :: GetBucketCors)

-- | The bucket name for which to get the cors configuration.
getBucketCors_bucket :: Lens.Lens' GetBucketCors BucketName
getBucketCors_bucket = Lens.lens (\GetBucketCors' {bucket} -> bucket) (\s@GetBucketCors' {} a -> s {bucket = a} :: GetBucketCors)

instance Core.AWSRequest GetBucketCors where
  type
    AWSResponse GetBucketCors =
      GetBucketCorsResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetBucketCorsResponse'
            Prelude.<$> (Core.may (Data.parseXMLList "CORSRule") x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBucketCors where
  hashWithSalt _salt GetBucketCors' {..} =
    _salt
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` bucket

instance Prelude.NFData GetBucketCors where
  rnf GetBucketCors' {..} =
    Prelude.rnf expectedBucketOwner `Prelude.seq`
      Prelude.rnf bucket

instance Data.ToHeaders GetBucketCors where
  toHeaders GetBucketCors' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner
      ]

instance Data.ToPath GetBucketCors where
  toPath GetBucketCors' {..} =
    Prelude.mconcat ["/", Data.toBS bucket]

instance Data.ToQuery GetBucketCors where
  toQuery = Prelude.const (Prelude.mconcat ["cors"])

-- | /See:/ 'newGetBucketCorsResponse' smart constructor.
data GetBucketCorsResponse = GetBucketCorsResponse'
  { -- | A set of origins and methods (cross-origin access that you want to
    -- allow). You can add up to 100 rules to the configuration.
    cORSRules :: Prelude.Maybe [CORSRule],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
getBucketCorsResponse_cORSRules = Lens.lens (\GetBucketCorsResponse' {cORSRules} -> cORSRules) (\s@GetBucketCorsResponse' {} a -> s {cORSRules = a} :: GetBucketCorsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getBucketCorsResponse_httpStatus :: Lens.Lens' GetBucketCorsResponse Prelude.Int
getBucketCorsResponse_httpStatus = Lens.lens (\GetBucketCorsResponse' {httpStatus} -> httpStatus) (\s@GetBucketCorsResponse' {} a -> s {httpStatus = a} :: GetBucketCorsResponse)

instance Prelude.NFData GetBucketCorsResponse where
  rnf GetBucketCorsResponse' {..} =
    Prelude.rnf cORSRules `Prelude.seq`
      Prelude.rnf httpStatus
