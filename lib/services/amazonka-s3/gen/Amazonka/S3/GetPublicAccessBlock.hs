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
-- Module      : Amazonka.S3.GetPublicAccessBlock
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the @PublicAccessBlock@ configuration for an Amazon S3 bucket.
-- To use this operation, you must have the @s3:GetBucketPublicAccessBlock@
-- permission. For more information about Amazon S3 permissions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html Specifying Permissions in a Policy>.
--
-- When Amazon S3 evaluates the @PublicAccessBlock@ configuration for a
-- bucket or an object, it checks the @PublicAccessBlock@ configuration for
-- both the bucket (or the bucket that contains the object) and the bucket
-- owner\'s account. If the @PublicAccessBlock@ settings are different
-- between the bucket and the account, Amazon S3 uses the most restrictive
-- combination of the bucket-level and account-level settings.
--
-- For more information about when Amazon S3 considers a bucket or an
-- object public, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html#access-control-block-public-access-policy-status The Meaning of \"Public\">.
--
-- The following operations are related to @GetPublicAccessBlock@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html Using Amazon S3 Block Public Access>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutPublicAccessBlock.html PutPublicAccessBlock>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetPublicAccessBlock.html GetPublicAccessBlock>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeletePublicAccessBlock.html DeletePublicAccessBlock>
module Amazonka.S3.GetPublicAccessBlock
  ( -- * Creating a Request
    GetPublicAccessBlock (..),
    newGetPublicAccessBlock,

    -- * Request Lenses
    getPublicAccessBlock_expectedBucketOwner,
    getPublicAccessBlock_bucket,

    -- * Destructuring the Response
    GetPublicAccessBlockResponse (..),
    newGetPublicAccessBlockResponse,

    -- * Response Lenses
    getPublicAccessBlockResponse_publicAccessBlockConfiguration,
    getPublicAccessBlockResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newGetPublicAccessBlock' smart constructor.
data GetPublicAccessBlock = GetPublicAccessBlock'
  { -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon S3 bucket whose @PublicAccessBlock@ configuration
    -- you want to retrieve.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPublicAccessBlock' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'getPublicAccessBlock_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'bucket', 'getPublicAccessBlock_bucket' - The name of the Amazon S3 bucket whose @PublicAccessBlock@ configuration
-- you want to retrieve.
newGetPublicAccessBlock ::
  -- | 'bucket'
  BucketName ->
  GetPublicAccessBlock
newGetPublicAccessBlock pBucket_ =
  GetPublicAccessBlock'
    { expectedBucketOwner =
        Prelude.Nothing,
      bucket = pBucket_
    }

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
getPublicAccessBlock_expectedBucketOwner :: Lens.Lens' GetPublicAccessBlock (Prelude.Maybe Prelude.Text)
getPublicAccessBlock_expectedBucketOwner = Lens.lens (\GetPublicAccessBlock' {expectedBucketOwner} -> expectedBucketOwner) (\s@GetPublicAccessBlock' {} a -> s {expectedBucketOwner = a} :: GetPublicAccessBlock)

-- | The name of the Amazon S3 bucket whose @PublicAccessBlock@ configuration
-- you want to retrieve.
getPublicAccessBlock_bucket :: Lens.Lens' GetPublicAccessBlock BucketName
getPublicAccessBlock_bucket = Lens.lens (\GetPublicAccessBlock' {bucket} -> bucket) (\s@GetPublicAccessBlock' {} a -> s {bucket = a} :: GetPublicAccessBlock)

instance Core.AWSRequest GetPublicAccessBlock where
  type
    AWSResponse GetPublicAccessBlock =
      GetPublicAccessBlockResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetPublicAccessBlockResponse'
            Prelude.<$> (Data.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPublicAccessBlock where
  hashWithSalt _salt GetPublicAccessBlock' {..} =
    _salt
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` bucket

instance Prelude.NFData GetPublicAccessBlock where
  rnf GetPublicAccessBlock' {..} =
    Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf bucket

instance Data.ToHeaders GetPublicAccessBlock where
  toHeaders GetPublicAccessBlock' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner
      ]

instance Data.ToPath GetPublicAccessBlock where
  toPath GetPublicAccessBlock' {..} =
    Prelude.mconcat ["/", Data.toBS bucket]

instance Data.ToQuery GetPublicAccessBlock where
  toQuery =
    Prelude.const
      (Prelude.mconcat ["publicAccessBlock"])

-- | /See:/ 'newGetPublicAccessBlockResponse' smart constructor.
data GetPublicAccessBlockResponse = GetPublicAccessBlockResponse'
  { -- | The @PublicAccessBlock@ configuration currently in effect for this
    -- Amazon S3 bucket.
    publicAccessBlockConfiguration :: Prelude.Maybe PublicAccessBlockConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPublicAccessBlockResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'publicAccessBlockConfiguration', 'getPublicAccessBlockResponse_publicAccessBlockConfiguration' - The @PublicAccessBlock@ configuration currently in effect for this
-- Amazon S3 bucket.
--
-- 'httpStatus', 'getPublicAccessBlockResponse_httpStatus' - The response's http status code.
newGetPublicAccessBlockResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPublicAccessBlockResponse
newGetPublicAccessBlockResponse pHttpStatus_ =
  GetPublicAccessBlockResponse'
    { publicAccessBlockConfiguration =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The @PublicAccessBlock@ configuration currently in effect for this
-- Amazon S3 bucket.
getPublicAccessBlockResponse_publicAccessBlockConfiguration :: Lens.Lens' GetPublicAccessBlockResponse (Prelude.Maybe PublicAccessBlockConfiguration)
getPublicAccessBlockResponse_publicAccessBlockConfiguration = Lens.lens (\GetPublicAccessBlockResponse' {publicAccessBlockConfiguration} -> publicAccessBlockConfiguration) (\s@GetPublicAccessBlockResponse' {} a -> s {publicAccessBlockConfiguration = a} :: GetPublicAccessBlockResponse)

-- | The response's http status code.
getPublicAccessBlockResponse_httpStatus :: Lens.Lens' GetPublicAccessBlockResponse Prelude.Int
getPublicAccessBlockResponse_httpStatus = Lens.lens (\GetPublicAccessBlockResponse' {httpStatus} -> httpStatus) (\s@GetPublicAccessBlockResponse' {} a -> s {httpStatus = a} :: GetPublicAccessBlockResponse)

instance Prelude.NFData GetPublicAccessBlockResponse where
  rnf GetPublicAccessBlockResponse' {..} =
    Prelude.rnf publicAccessBlockConfiguration
      `Prelude.seq` Prelude.rnf httpStatus
