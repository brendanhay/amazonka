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
-- Module      : Network.AWS.S3.GetBucketLifecycleConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Bucket lifecycle configuration now supports specifying a lifecycle rule
-- using an object key name prefix, one or more object tags, or a
-- combination of both. Accordingly, this section describes the latest API.
-- The response describes the new filter element that you can use to
-- specify a filter to select a subset of objects to which the rule
-- applies. If you are using a previous version of the lifecycle
-- configuration, it still works. For the earlier API description, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketLifecycle.html GetBucketLifecycle>.
--
-- Returns the lifecycle configuration information set on the bucket. For
-- information about lifecycle configuration, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lifecycle-mgmt.html Object Lifecycle Management>.
--
-- To use this operation, you must have permission to perform the
-- @s3:GetLifecycleConfiguration@ action. The bucket owner has this
-- permission, by default. The bucket owner can grant this permission to
-- others. For more information about permissions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations>
-- and
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources>.
--
-- @GetBucketLifecycleConfiguration@ has the following special error:
--
-- -   Error code: @NoSuchLifecycleConfiguration@
--
--     -   Description: The lifecycle configuration does not exist.
--
--     -   HTTP Status Code: 404 Not Found
--
--     -   SOAP Fault Code Prefix: Client
--
-- The following operations are related to
-- @GetBucketLifecycleConfiguration@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketLifecycle.html GetBucketLifecycle>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketLifecycle.html PutBucketLifecycle>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketLifecycle.html DeleteBucketLifecycle>
module Network.AWS.S3.GetBucketLifecycleConfiguration
  ( -- * Creating a Request
    GetBucketLifecycleConfiguration (..),
    newGetBucketLifecycleConfiguration,

    -- * Request Lenses
    getBucketLifecycleConfiguration_expectedBucketOwner,
    getBucketLifecycleConfiguration_bucket,

    -- * Destructuring the Response
    GetBucketLifecycleConfigurationResponse (..),
    newGetBucketLifecycleConfigurationResponse,

    -- * Response Lenses
    getBucketLifecycleConfigurationResponse_rules,
    getBucketLifecycleConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newGetBucketLifecycleConfiguration' smart constructor.
data GetBucketLifecycleConfiguration = GetBucketLifecycleConfiguration'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Core.Text,
    -- | The name of the bucket for which to get the lifecycle information.
    bucket :: BucketName
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetBucketLifecycleConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'getBucketLifecycleConfiguration_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'bucket', 'getBucketLifecycleConfiguration_bucket' - The name of the bucket for which to get the lifecycle information.
newGetBucketLifecycleConfiguration ::
  -- | 'bucket'
  BucketName ->
  GetBucketLifecycleConfiguration
newGetBucketLifecycleConfiguration pBucket_ =
  GetBucketLifecycleConfiguration'
    { expectedBucketOwner =
        Core.Nothing,
      bucket = pBucket_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
getBucketLifecycleConfiguration_expectedBucketOwner :: Lens.Lens' GetBucketLifecycleConfiguration (Core.Maybe Core.Text)
getBucketLifecycleConfiguration_expectedBucketOwner = Lens.lens (\GetBucketLifecycleConfiguration' {expectedBucketOwner} -> expectedBucketOwner) (\s@GetBucketLifecycleConfiguration' {} a -> s {expectedBucketOwner = a} :: GetBucketLifecycleConfiguration)

-- | The name of the bucket for which to get the lifecycle information.
getBucketLifecycleConfiguration_bucket :: Lens.Lens' GetBucketLifecycleConfiguration BucketName
getBucketLifecycleConfiguration_bucket = Lens.lens (\GetBucketLifecycleConfiguration' {bucket} -> bucket) (\s@GetBucketLifecycleConfiguration' {} a -> s {bucket = a} :: GetBucketLifecycleConfiguration)

instance
  Core.AWSRequest
    GetBucketLifecycleConfiguration
  where
  type
    AWSResponse GetBucketLifecycleConfiguration =
      GetBucketLifecycleConfigurationResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetBucketLifecycleConfigurationResponse'
            Core.<$> (Core.may (Core.parseXMLList "Rule") x)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    GetBucketLifecycleConfiguration

instance Core.NFData GetBucketLifecycleConfiguration

instance
  Core.ToHeaders
    GetBucketLifecycleConfiguration
  where
  toHeaders GetBucketLifecycleConfiguration' {..} =
    Core.mconcat
      [ "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner
      ]

instance Core.ToPath GetBucketLifecycleConfiguration where
  toPath GetBucketLifecycleConfiguration' {..} =
    Core.mconcat ["/", Core.toBS bucket]

instance Core.ToQuery GetBucketLifecycleConfiguration where
  toQuery = Core.const (Core.mconcat ["lifecycle"])

-- | /See:/ 'newGetBucketLifecycleConfigurationResponse' smart constructor.
data GetBucketLifecycleConfigurationResponse = GetBucketLifecycleConfigurationResponse'
  { -- | Container for a lifecycle rule.
    rules :: Core.Maybe [LifecycleRule],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetBucketLifecycleConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rules', 'getBucketLifecycleConfigurationResponse_rules' - Container for a lifecycle rule.
--
-- 'httpStatus', 'getBucketLifecycleConfigurationResponse_httpStatus' - The response's http status code.
newGetBucketLifecycleConfigurationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetBucketLifecycleConfigurationResponse
newGetBucketLifecycleConfigurationResponse
  pHttpStatus_ =
    GetBucketLifecycleConfigurationResponse'
      { rules =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Container for a lifecycle rule.
getBucketLifecycleConfigurationResponse_rules :: Lens.Lens' GetBucketLifecycleConfigurationResponse (Core.Maybe [LifecycleRule])
getBucketLifecycleConfigurationResponse_rules = Lens.lens (\GetBucketLifecycleConfigurationResponse' {rules} -> rules) (\s@GetBucketLifecycleConfigurationResponse' {} a -> s {rules = a} :: GetBucketLifecycleConfigurationResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getBucketLifecycleConfigurationResponse_httpStatus :: Lens.Lens' GetBucketLifecycleConfigurationResponse Core.Int
getBucketLifecycleConfigurationResponse_httpStatus = Lens.lens (\GetBucketLifecycleConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetBucketLifecycleConfigurationResponse' {} a -> s {httpStatus = a} :: GetBucketLifecycleConfigurationResponse)

instance
  Core.NFData
    GetBucketLifecycleConfigurationResponse
