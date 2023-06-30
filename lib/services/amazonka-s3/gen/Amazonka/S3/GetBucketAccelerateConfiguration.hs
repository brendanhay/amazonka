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
-- Module      : Amazonka.S3.GetBucketAccelerateConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This implementation of the GET action uses the @accelerate@ subresource
-- to return the Transfer Acceleration state of a bucket, which is either
-- @Enabled@ or @Suspended@. Amazon S3 Transfer Acceleration is a
-- bucket-level feature that enables you to perform faster data transfers
-- to and from Amazon S3.
--
-- To use this operation, you must have permission to perform the
-- @s3:GetAccelerateConfiguration@ action. The bucket owner has this
-- permission by default. The bucket owner can grant this permission to
-- others. For more information about permissions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations>
-- and
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/s3-access-control.html Managing Access Permissions to your Amazon S3 Resources>
-- in the /Amazon S3 User Guide/.
--
-- You set the Transfer Acceleration state of an existing bucket to
-- @Enabled@ or @Suspended@ by using the
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketAccelerateConfiguration.html PutBucketAccelerateConfiguration>
-- operation.
--
-- A GET @accelerate@ request does not return a state value for a bucket
-- that has no transfer acceleration state. A bucket has no Transfer
-- Acceleration state if a state has never been set on the bucket.
--
-- For more information about transfer acceleration, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/transfer-acceleration.html Transfer Acceleration>
-- in the Amazon S3 User Guide.
--
-- __Related Resources__
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketAccelerateConfiguration.html PutBucketAccelerateConfiguration>
module Amazonka.S3.GetBucketAccelerateConfiguration
  ( -- * Creating a Request
    GetBucketAccelerateConfiguration (..),
    newGetBucketAccelerateConfiguration,

    -- * Request Lenses
    getBucketAccelerateConfiguration_expectedBucketOwner,
    getBucketAccelerateConfiguration_bucket,

    -- * Destructuring the Response
    GetBucketAccelerateConfigurationResponse (..),
    newGetBucketAccelerateConfigurationResponse,

    -- * Response Lenses
    getBucketAccelerateConfigurationResponse_status,
    getBucketAccelerateConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newGetBucketAccelerateConfiguration' smart constructor.
data GetBucketAccelerateConfiguration = GetBucketAccelerateConfiguration'
  { -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the bucket for which the accelerate configuration is
    -- retrieved.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBucketAccelerateConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'getBucketAccelerateConfiguration_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'bucket', 'getBucketAccelerateConfiguration_bucket' - The name of the bucket for which the accelerate configuration is
-- retrieved.
newGetBucketAccelerateConfiguration ::
  -- | 'bucket'
  BucketName ->
  GetBucketAccelerateConfiguration
newGetBucketAccelerateConfiguration pBucket_ =
  GetBucketAccelerateConfiguration'
    { expectedBucketOwner =
        Prelude.Nothing,
      bucket = pBucket_
    }

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
getBucketAccelerateConfiguration_expectedBucketOwner :: Lens.Lens' GetBucketAccelerateConfiguration (Prelude.Maybe Prelude.Text)
getBucketAccelerateConfiguration_expectedBucketOwner = Lens.lens (\GetBucketAccelerateConfiguration' {expectedBucketOwner} -> expectedBucketOwner) (\s@GetBucketAccelerateConfiguration' {} a -> s {expectedBucketOwner = a} :: GetBucketAccelerateConfiguration)

-- | The name of the bucket for which the accelerate configuration is
-- retrieved.
getBucketAccelerateConfiguration_bucket :: Lens.Lens' GetBucketAccelerateConfiguration BucketName
getBucketAccelerateConfiguration_bucket = Lens.lens (\GetBucketAccelerateConfiguration' {bucket} -> bucket) (\s@GetBucketAccelerateConfiguration' {} a -> s {bucket = a} :: GetBucketAccelerateConfiguration)

instance
  Core.AWSRequest
    GetBucketAccelerateConfiguration
  where
  type
    AWSResponse GetBucketAccelerateConfiguration =
      GetBucketAccelerateConfigurationResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetBucketAccelerateConfigurationResponse'
            Prelude.<$> (x Data..@? "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetBucketAccelerateConfiguration
  where
  hashWithSalt
    _salt
    GetBucketAccelerateConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` expectedBucketOwner
        `Prelude.hashWithSalt` bucket

instance
  Prelude.NFData
    GetBucketAccelerateConfiguration
  where
  rnf GetBucketAccelerateConfiguration' {..} =
    Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf bucket

instance
  Data.ToHeaders
    GetBucketAccelerateConfiguration
  where
  toHeaders GetBucketAccelerateConfiguration' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner
      ]

instance Data.ToPath GetBucketAccelerateConfiguration where
  toPath GetBucketAccelerateConfiguration' {..} =
    Prelude.mconcat ["/", Data.toBS bucket]

instance
  Data.ToQuery
    GetBucketAccelerateConfiguration
  where
  toQuery =
    Prelude.const (Prelude.mconcat ["accelerate"])

-- | /See:/ 'newGetBucketAccelerateConfigurationResponse' smart constructor.
data GetBucketAccelerateConfigurationResponse = GetBucketAccelerateConfigurationResponse'
  { -- | The accelerate configuration of the bucket.
    status :: Prelude.Maybe BucketAccelerateStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBucketAccelerateConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'getBucketAccelerateConfigurationResponse_status' - The accelerate configuration of the bucket.
--
-- 'httpStatus', 'getBucketAccelerateConfigurationResponse_httpStatus' - The response's http status code.
newGetBucketAccelerateConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBucketAccelerateConfigurationResponse
newGetBucketAccelerateConfigurationResponse
  pHttpStatus_ =
    GetBucketAccelerateConfigurationResponse'
      { status =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The accelerate configuration of the bucket.
getBucketAccelerateConfigurationResponse_status :: Lens.Lens' GetBucketAccelerateConfigurationResponse (Prelude.Maybe BucketAccelerateStatus)
getBucketAccelerateConfigurationResponse_status = Lens.lens (\GetBucketAccelerateConfigurationResponse' {status} -> status) (\s@GetBucketAccelerateConfigurationResponse' {} a -> s {status = a} :: GetBucketAccelerateConfigurationResponse)

-- | The response's http status code.
getBucketAccelerateConfigurationResponse_httpStatus :: Lens.Lens' GetBucketAccelerateConfigurationResponse Prelude.Int
getBucketAccelerateConfigurationResponse_httpStatus = Lens.lens (\GetBucketAccelerateConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetBucketAccelerateConfigurationResponse' {} a -> s {httpStatus = a} :: GetBucketAccelerateConfigurationResponse)

instance
  Prelude.NFData
    GetBucketAccelerateConfigurationResponse
  where
  rnf GetBucketAccelerateConfigurationResponse' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
