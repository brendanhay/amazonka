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
-- Module      : Network.AWS.S3.GetBucketEncryption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the default encryption configuration for an Amazon S3 bucket. If
-- the bucket does not have a default encryption configuration,
-- GetBucketEncryption returns
-- @ServerSideEncryptionConfigurationNotFoundError@.
--
-- For information about the Amazon S3 default encryption feature, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-encryption.html Amazon S3 Default Bucket Encryption>.
--
-- To use this operation, you must have permission to perform the
-- @s3:GetEncryptionConfiguration@ action. The bucket owner has this
-- permission by default. The bucket owner can grant this permission to
-- others. For more information about permissions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations>
-- and
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources>.
--
-- The following operations are related to @GetBucketEncryption@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketEncryption.html PutBucketEncryption>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketEncryption.html DeleteBucketEncryption>
module Network.AWS.S3.GetBucketEncryption
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newGetBucketEncryption' smart constructor.
data GetBucketEncryption = GetBucketEncryption'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Core.Text,
    -- | The name of the bucket from which the server-side encryption
    -- configuration is retrieved.
    bucket :: BucketName
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetBucketEncryption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'getBucketEncryption_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
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
        Core.Nothing,
      bucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
getBucketEncryption_expectedBucketOwner :: Lens.Lens' GetBucketEncryption (Core.Maybe Core.Text)
getBucketEncryption_expectedBucketOwner = Lens.lens (\GetBucketEncryption' {expectedBucketOwner} -> expectedBucketOwner) (\s@GetBucketEncryption' {} a -> s {expectedBucketOwner = a} :: GetBucketEncryption)

-- | The name of the bucket from which the server-side encryption
-- configuration is retrieved.
getBucketEncryption_bucket :: Lens.Lens' GetBucketEncryption BucketName
getBucketEncryption_bucket = Lens.lens (\GetBucketEncryption' {bucket} -> bucket) (\s@GetBucketEncryption' {} a -> s {bucket = a} :: GetBucketEncryption)

instance Core.AWSRequest GetBucketEncryption where
  type
    AWSResponse GetBucketEncryption =
      GetBucketEncryptionResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetBucketEncryptionResponse'
            Core.<$> (Core.parseXML x)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetBucketEncryption

instance Core.NFData GetBucketEncryption

instance Core.ToHeaders GetBucketEncryption where
  toHeaders GetBucketEncryption' {..} =
    Core.mconcat
      [ "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner
      ]

instance Core.ToPath GetBucketEncryption where
  toPath GetBucketEncryption' {..} =
    Core.mconcat ["/", Core.toBS bucket]

instance Core.ToQuery GetBucketEncryption where
  toQuery = Core.const (Core.mconcat ["encryption"])

-- | /See:/ 'newGetBucketEncryptionResponse' smart constructor.
data GetBucketEncryptionResponse = GetBucketEncryptionResponse'
  { serverSideEncryptionConfiguration :: Core.Maybe ServerSideEncryptionConfiguration,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
  Core.Int ->
  GetBucketEncryptionResponse
newGetBucketEncryptionResponse pHttpStatus_ =
  GetBucketEncryptionResponse'
    { serverSideEncryptionConfiguration =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getBucketEncryptionResponse_serverSideEncryptionConfiguration :: Lens.Lens' GetBucketEncryptionResponse (Core.Maybe ServerSideEncryptionConfiguration)
getBucketEncryptionResponse_serverSideEncryptionConfiguration = Lens.lens (\GetBucketEncryptionResponse' {serverSideEncryptionConfiguration} -> serverSideEncryptionConfiguration) (\s@GetBucketEncryptionResponse' {} a -> s {serverSideEncryptionConfiguration = a} :: GetBucketEncryptionResponse)

-- | The response's http status code.
getBucketEncryptionResponse_httpStatus :: Lens.Lens' GetBucketEncryptionResponse Core.Int
getBucketEncryptionResponse_httpStatus = Lens.lens (\GetBucketEncryptionResponse' {httpStatus} -> httpStatus) (\s@GetBucketEncryptionResponse' {} a -> s {httpStatus = a} :: GetBucketEncryptionResponse)

instance Core.NFData GetBucketEncryptionResponse
