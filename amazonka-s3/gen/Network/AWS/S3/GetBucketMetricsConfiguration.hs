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
-- Module      : Network.AWS.S3.GetBucketMetricsConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a metrics configuration (specified by the metrics configuration ID)
-- from the bucket. Note that this doesn\'t include the daily storage
-- metrics.
--
-- To use this operation, you must have permissions to perform the
-- @s3:GetMetricsConfiguration@ action. The bucket owner has this
-- permission by default. The bucket owner can grant this permission to
-- others. For more information about permissions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations>
-- and
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources>.
--
-- For information about CloudWatch request metrics for Amazon S3, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/cloudwatch-monitoring.html Monitoring Metrics with Amazon CloudWatch>.
--
-- The following operations are related to @GetBucketMetricsConfiguration@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketMetricsConfiguration.html PutBucketMetricsConfiguration>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketMetricsConfiguration.html DeleteBucketMetricsConfiguration>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketMetricsConfigurations.html ListBucketMetricsConfigurations>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/dev/cloudwatch-monitoring.html Monitoring Metrics with Amazon CloudWatch>
module Network.AWS.S3.GetBucketMetricsConfiguration
  ( -- * Creating a Request
    GetBucketMetricsConfiguration (..),
    newGetBucketMetricsConfiguration,

    -- * Request Lenses
    getBucketMetricsConfiguration_expectedBucketOwner,
    getBucketMetricsConfiguration_bucket,
    getBucketMetricsConfiguration_id,

    -- * Destructuring the Response
    GetBucketMetricsConfigurationResponse (..),
    newGetBucketMetricsConfigurationResponse,

    -- * Response Lenses
    getBucketMetricsConfigurationResponse_metricsConfiguration,
    getBucketMetricsConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newGetBucketMetricsConfiguration' smart constructor.
data GetBucketMetricsConfiguration = GetBucketMetricsConfiguration'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Core.Text,
    -- | The name of the bucket containing the metrics configuration to retrieve.
    bucket :: BucketName,
    -- | The ID used to identify the metrics configuration.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetBucketMetricsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'getBucketMetricsConfiguration_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'bucket', 'getBucketMetricsConfiguration_bucket' - The name of the bucket containing the metrics configuration to retrieve.
--
-- 'id', 'getBucketMetricsConfiguration_id' - The ID used to identify the metrics configuration.
newGetBucketMetricsConfiguration ::
  -- | 'bucket'
  BucketName ->
  -- | 'id'
  Core.Text ->
  GetBucketMetricsConfiguration
newGetBucketMetricsConfiguration pBucket_ pId_ =
  GetBucketMetricsConfiguration'
    { expectedBucketOwner =
        Core.Nothing,
      bucket = pBucket_,
      id = pId_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
getBucketMetricsConfiguration_expectedBucketOwner :: Lens.Lens' GetBucketMetricsConfiguration (Core.Maybe Core.Text)
getBucketMetricsConfiguration_expectedBucketOwner = Lens.lens (\GetBucketMetricsConfiguration' {expectedBucketOwner} -> expectedBucketOwner) (\s@GetBucketMetricsConfiguration' {} a -> s {expectedBucketOwner = a} :: GetBucketMetricsConfiguration)

-- | The name of the bucket containing the metrics configuration to retrieve.
getBucketMetricsConfiguration_bucket :: Lens.Lens' GetBucketMetricsConfiguration BucketName
getBucketMetricsConfiguration_bucket = Lens.lens (\GetBucketMetricsConfiguration' {bucket} -> bucket) (\s@GetBucketMetricsConfiguration' {} a -> s {bucket = a} :: GetBucketMetricsConfiguration)

-- | The ID used to identify the metrics configuration.
getBucketMetricsConfiguration_id :: Lens.Lens' GetBucketMetricsConfiguration Core.Text
getBucketMetricsConfiguration_id = Lens.lens (\GetBucketMetricsConfiguration' {id} -> id) (\s@GetBucketMetricsConfiguration' {} a -> s {id = a} :: GetBucketMetricsConfiguration)

instance
  Core.AWSRequest
    GetBucketMetricsConfiguration
  where
  type
    AWSResponse GetBucketMetricsConfiguration =
      GetBucketMetricsConfigurationResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetBucketMetricsConfigurationResponse'
            Core.<$> (Core.parseXML x)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetBucketMetricsConfiguration

instance Core.NFData GetBucketMetricsConfiguration

instance Core.ToHeaders GetBucketMetricsConfiguration where
  toHeaders GetBucketMetricsConfiguration' {..} =
    Core.mconcat
      [ "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner
      ]

instance Core.ToPath GetBucketMetricsConfiguration where
  toPath GetBucketMetricsConfiguration' {..} =
    Core.mconcat ["/", Core.toBS bucket]

instance Core.ToQuery GetBucketMetricsConfiguration where
  toQuery GetBucketMetricsConfiguration' {..} =
    Core.mconcat ["id" Core.=: id, "metrics"]

-- | /See:/ 'newGetBucketMetricsConfigurationResponse' smart constructor.
data GetBucketMetricsConfigurationResponse = GetBucketMetricsConfigurationResponse'
  { -- | Specifies the metrics configuration.
    metricsConfiguration :: Core.Maybe MetricsConfiguration,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetBucketMetricsConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricsConfiguration', 'getBucketMetricsConfigurationResponse_metricsConfiguration' - Specifies the metrics configuration.
--
-- 'httpStatus', 'getBucketMetricsConfigurationResponse_httpStatus' - The response's http status code.
newGetBucketMetricsConfigurationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetBucketMetricsConfigurationResponse
newGetBucketMetricsConfigurationResponse pHttpStatus_ =
  GetBucketMetricsConfigurationResponse'
    { metricsConfiguration =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Specifies the metrics configuration.
getBucketMetricsConfigurationResponse_metricsConfiguration :: Lens.Lens' GetBucketMetricsConfigurationResponse (Core.Maybe MetricsConfiguration)
getBucketMetricsConfigurationResponse_metricsConfiguration = Lens.lens (\GetBucketMetricsConfigurationResponse' {metricsConfiguration} -> metricsConfiguration) (\s@GetBucketMetricsConfigurationResponse' {} a -> s {metricsConfiguration = a} :: GetBucketMetricsConfigurationResponse)

-- | The response's http status code.
getBucketMetricsConfigurationResponse_httpStatus :: Lens.Lens' GetBucketMetricsConfigurationResponse Core.Int
getBucketMetricsConfigurationResponse_httpStatus = Lens.lens (\GetBucketMetricsConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetBucketMetricsConfigurationResponse' {} a -> s {httpStatus = a} :: GetBucketMetricsConfigurationResponse)

instance
  Core.NFData
    GetBucketMetricsConfigurationResponse
