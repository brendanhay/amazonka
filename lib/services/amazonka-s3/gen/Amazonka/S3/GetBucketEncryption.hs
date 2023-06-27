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
-- Module      : Amazonka.S3.GetBucketEncryption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the default encryption configuration for an Amazon S3 bucket. By
-- default, all buckets have a default encryption configuration that uses
-- server-side encryption with Amazon S3 managed keys (SSE-S3). For
-- information about the bucket default encryption feature, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-encryption.html Amazon S3 Bucket Default Encryption>
-- in the /Amazon S3 User Guide/.
--
-- To use this operation, you must have permission to perform the
-- @s3:GetEncryptionConfiguration@ action. The bucket owner has this
-- permission by default. The bucket owner can grant this permission to
-- others. For more information about permissions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations>
-- and
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources>.
--
-- The following operations are related to @GetBucketEncryption@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketEncryption.html PutBucketEncryption>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketEncryption.html DeleteBucketEncryption>
module Amazonka.S3.GetBucketEncryption
  ( -- * Creating a Request
    GetBucketEncryption (..),
    newGetBucketEncryption,

    -- * Request Lenses
    getBucketEncryption_expectedBucketOwner,
    getBucketEncryption_bucket,

    -- * Destructuring the Response
    GetBucketEncryptionResponse (..),
    newGetBucketEncryptionResponse,

    -- * Response Lenses
    getBucketEncryptionResponse_serverSideEncryptionConfiguration,
    getBucketEncryptionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newGetBucketEncryption' smart constructor.
data GetBucketEncryption = GetBucketEncryption'
  { -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the bucket from which the server-side encryption
    -- configuration is retrieved.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBucketEncryption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'getBucketEncryption_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'bucket', 'getBucketEncryption_bucket' - The name of the bucket from which the server-side encryption
-- configuration is retrieved.
newGetBucketEncryption ::
  -- | 'bucket'
  BucketName ->
  GetBucketEncryption
newGetBucketEncryption pBucket_ =
  GetBucketEncryption'
    { expectedBucketOwner =
        Prelude.Nothing,
      bucket = pBucket_
    }

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
getBucketEncryption_expectedBucketOwner :: Lens.Lens' GetBucketEncryption (Prelude.Maybe Prelude.Text)
getBucketEncryption_expectedBucketOwner = Lens.lens (\GetBucketEncryption' {expectedBucketOwner} -> expectedBucketOwner) (\s@GetBucketEncryption' {} a -> s {expectedBucketOwner = a} :: GetBucketEncryption)

-- | The name of the bucket from which the server-side encryption
-- configuration is retrieved.
getBucketEncryption_bucket :: Lens.Lens' GetBucketEncryption BucketName
getBucketEncryption_bucket = Lens.lens (\GetBucketEncryption' {bucket} -> bucket) (\s@GetBucketEncryption' {} a -> s {bucket = a} :: GetBucketEncryption)

instance Core.AWSRequest GetBucketEncryption where
  type
    AWSResponse GetBucketEncryption =
      GetBucketEncryptionResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetBucketEncryptionResponse'
            Prelude.<$> (Data.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBucketEncryption where
  hashWithSalt _salt GetBucketEncryption' {..} =
    _salt
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` bucket

instance Prelude.NFData GetBucketEncryption where
  rnf GetBucketEncryption' {..} =
    Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf bucket

instance Data.ToHeaders GetBucketEncryption where
  toHeaders GetBucketEncryption' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner
      ]

instance Data.ToPath GetBucketEncryption where
  toPath GetBucketEncryption' {..} =
    Prelude.mconcat ["/", Data.toBS bucket]

instance Data.ToQuery GetBucketEncryption where
  toQuery =
    Prelude.const (Prelude.mconcat ["encryption"])

-- | /See:/ 'newGetBucketEncryptionResponse' smart constructor.
data GetBucketEncryptionResponse = GetBucketEncryptionResponse'
  { serverSideEncryptionConfiguration :: Prelude.Maybe ServerSideEncryptionConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBucketEncryptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverSideEncryptionConfiguration', 'getBucketEncryptionResponse_serverSideEncryptionConfiguration' - Undocumented member.
--
-- 'httpStatus', 'getBucketEncryptionResponse_httpStatus' - The response's http status code.
newGetBucketEncryptionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBucketEncryptionResponse
newGetBucketEncryptionResponse pHttpStatus_ =
  GetBucketEncryptionResponse'
    { serverSideEncryptionConfiguration =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getBucketEncryptionResponse_serverSideEncryptionConfiguration :: Lens.Lens' GetBucketEncryptionResponse (Prelude.Maybe ServerSideEncryptionConfiguration)
getBucketEncryptionResponse_serverSideEncryptionConfiguration = Lens.lens (\GetBucketEncryptionResponse' {serverSideEncryptionConfiguration} -> serverSideEncryptionConfiguration) (\s@GetBucketEncryptionResponse' {} a -> s {serverSideEncryptionConfiguration = a} :: GetBucketEncryptionResponse)

-- | The response's http status code.
getBucketEncryptionResponse_httpStatus :: Lens.Lens' GetBucketEncryptionResponse Prelude.Int
getBucketEncryptionResponse_httpStatus = Lens.lens (\GetBucketEncryptionResponse' {httpStatus} -> httpStatus) (\s@GetBucketEncryptionResponse' {} a -> s {httpStatus = a} :: GetBucketEncryptionResponse)

instance Prelude.NFData GetBucketEncryptionResponse where
  rnf GetBucketEncryptionResponse' {..} =
    Prelude.rnf serverSideEncryptionConfiguration
      `Prelude.seq` Prelude.rnf httpStatus
