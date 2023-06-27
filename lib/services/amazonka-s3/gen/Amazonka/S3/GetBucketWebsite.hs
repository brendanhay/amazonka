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
-- Module      : Amazonka.S3.GetBucketWebsite
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the website configuration for a bucket. To host website on
-- Amazon S3, you can configure a bucket as website by adding a website
-- configuration. For more information about hosting websites, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/WebsiteHosting.html Hosting Websites on Amazon S3>.
--
-- This GET action requires the @S3:GetBucketWebsite@ permission. By
-- default, only the bucket owner can read the bucket website
-- configuration. However, bucket owners can allow other users to read the
-- website configuration by writing a bucket policy granting them the
-- @S3:GetBucketWebsite@ permission.
--
-- The following operations are related to @GetBucketWebsite@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketWebsite.html DeleteBucketWebsite>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketWebsite.html PutBucketWebsite>
module Amazonka.S3.GetBucketWebsite
  ( -- * Creating a Request
    GetBucketWebsite (..),
    newGetBucketWebsite,

    -- * Request Lenses
    getBucketWebsite_expectedBucketOwner,
    getBucketWebsite_bucket,

    -- * Destructuring the Response
    GetBucketWebsiteResponse (..),
    newGetBucketWebsiteResponse,

    -- * Response Lenses
    getBucketWebsiteResponse_errorDocument,
    getBucketWebsiteResponse_indexDocument,
    getBucketWebsiteResponse_redirectAllRequestsTo,
    getBucketWebsiteResponse_routingRules,
    getBucketWebsiteResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newGetBucketWebsite' smart constructor.
data GetBucketWebsite = GetBucketWebsite'
  { -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The bucket name for which to get the website configuration.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBucketWebsite' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'getBucketWebsite_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'bucket', 'getBucketWebsite_bucket' - The bucket name for which to get the website configuration.
newGetBucketWebsite ::
  -- | 'bucket'
  BucketName ->
  GetBucketWebsite
newGetBucketWebsite pBucket_ =
  GetBucketWebsite'
    { expectedBucketOwner =
        Prelude.Nothing,
      bucket = pBucket_
    }

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
getBucketWebsite_expectedBucketOwner :: Lens.Lens' GetBucketWebsite (Prelude.Maybe Prelude.Text)
getBucketWebsite_expectedBucketOwner = Lens.lens (\GetBucketWebsite' {expectedBucketOwner} -> expectedBucketOwner) (\s@GetBucketWebsite' {} a -> s {expectedBucketOwner = a} :: GetBucketWebsite)

-- | The bucket name for which to get the website configuration.
getBucketWebsite_bucket :: Lens.Lens' GetBucketWebsite BucketName
getBucketWebsite_bucket = Lens.lens (\GetBucketWebsite' {bucket} -> bucket) (\s@GetBucketWebsite' {} a -> s {bucket = a} :: GetBucketWebsite)

instance Core.AWSRequest GetBucketWebsite where
  type
    AWSResponse GetBucketWebsite =
      GetBucketWebsiteResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetBucketWebsiteResponse'
            Prelude.<$> (x Data..@? "ErrorDocument")
            Prelude.<*> (x Data..@? "IndexDocument")
            Prelude.<*> (x Data..@? "RedirectAllRequestsTo")
            Prelude.<*> ( x
                            Data..@? "RoutingRules"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "RoutingRule")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBucketWebsite where
  hashWithSalt _salt GetBucketWebsite' {..} =
    _salt
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` bucket

instance Prelude.NFData GetBucketWebsite where
  rnf GetBucketWebsite' {..} =
    Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf bucket

instance Data.ToHeaders GetBucketWebsite where
  toHeaders GetBucketWebsite' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner
      ]

instance Data.ToPath GetBucketWebsite where
  toPath GetBucketWebsite' {..} =
    Prelude.mconcat ["/", Data.toBS bucket]

instance Data.ToQuery GetBucketWebsite where
  toQuery = Prelude.const (Prelude.mconcat ["website"])

-- | /See:/ 'newGetBucketWebsiteResponse' smart constructor.
data GetBucketWebsiteResponse = GetBucketWebsiteResponse'
  { -- | The object key name of the website error document to use for 4XX class
    -- errors.
    errorDocument :: Prelude.Maybe ErrorDocument,
    -- | The name of the index document for the website (for example
    -- @index.html@).
    indexDocument :: Prelude.Maybe IndexDocument,
    -- | Specifies the redirect behavior of all requests to a website endpoint of
    -- an Amazon S3 bucket.
    redirectAllRequestsTo :: Prelude.Maybe RedirectAllRequestsTo,
    -- | Rules that define when a redirect is applied and the redirect behavior.
    routingRules :: Prelude.Maybe [RoutingRule],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBucketWebsiteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorDocument', 'getBucketWebsiteResponse_errorDocument' - The object key name of the website error document to use for 4XX class
-- errors.
--
-- 'indexDocument', 'getBucketWebsiteResponse_indexDocument' - The name of the index document for the website (for example
-- @index.html@).
--
-- 'redirectAllRequestsTo', 'getBucketWebsiteResponse_redirectAllRequestsTo' - Specifies the redirect behavior of all requests to a website endpoint of
-- an Amazon S3 bucket.
--
-- 'routingRules', 'getBucketWebsiteResponse_routingRules' - Rules that define when a redirect is applied and the redirect behavior.
--
-- 'httpStatus', 'getBucketWebsiteResponse_httpStatus' - The response's http status code.
newGetBucketWebsiteResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBucketWebsiteResponse
newGetBucketWebsiteResponse pHttpStatus_ =
  GetBucketWebsiteResponse'
    { errorDocument =
        Prelude.Nothing,
      indexDocument = Prelude.Nothing,
      redirectAllRequestsTo = Prelude.Nothing,
      routingRules = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The object key name of the website error document to use for 4XX class
-- errors.
getBucketWebsiteResponse_errorDocument :: Lens.Lens' GetBucketWebsiteResponse (Prelude.Maybe ErrorDocument)
getBucketWebsiteResponse_errorDocument = Lens.lens (\GetBucketWebsiteResponse' {errorDocument} -> errorDocument) (\s@GetBucketWebsiteResponse' {} a -> s {errorDocument = a} :: GetBucketWebsiteResponse)

-- | The name of the index document for the website (for example
-- @index.html@).
getBucketWebsiteResponse_indexDocument :: Lens.Lens' GetBucketWebsiteResponse (Prelude.Maybe IndexDocument)
getBucketWebsiteResponse_indexDocument = Lens.lens (\GetBucketWebsiteResponse' {indexDocument} -> indexDocument) (\s@GetBucketWebsiteResponse' {} a -> s {indexDocument = a} :: GetBucketWebsiteResponse)

-- | Specifies the redirect behavior of all requests to a website endpoint of
-- an Amazon S3 bucket.
getBucketWebsiteResponse_redirectAllRequestsTo :: Lens.Lens' GetBucketWebsiteResponse (Prelude.Maybe RedirectAllRequestsTo)
getBucketWebsiteResponse_redirectAllRequestsTo = Lens.lens (\GetBucketWebsiteResponse' {redirectAllRequestsTo} -> redirectAllRequestsTo) (\s@GetBucketWebsiteResponse' {} a -> s {redirectAllRequestsTo = a} :: GetBucketWebsiteResponse)

-- | Rules that define when a redirect is applied and the redirect behavior.
getBucketWebsiteResponse_routingRules :: Lens.Lens' GetBucketWebsiteResponse (Prelude.Maybe [RoutingRule])
getBucketWebsiteResponse_routingRules = Lens.lens (\GetBucketWebsiteResponse' {routingRules} -> routingRules) (\s@GetBucketWebsiteResponse' {} a -> s {routingRules = a} :: GetBucketWebsiteResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getBucketWebsiteResponse_httpStatus :: Lens.Lens' GetBucketWebsiteResponse Prelude.Int
getBucketWebsiteResponse_httpStatus = Lens.lens (\GetBucketWebsiteResponse' {httpStatus} -> httpStatus) (\s@GetBucketWebsiteResponse' {} a -> s {httpStatus = a} :: GetBucketWebsiteResponse)

instance Prelude.NFData GetBucketWebsiteResponse where
  rnf GetBucketWebsiteResponse' {..} =
    Prelude.rnf errorDocument
      `Prelude.seq` Prelude.rnf indexDocument
      `Prelude.seq` Prelude.rnf redirectAllRequestsTo
      `Prelude.seq` Prelude.rnf routingRules
      `Prelude.seq` Prelude.rnf httpStatus
