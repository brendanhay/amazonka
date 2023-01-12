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
-- Module      : Amazonka.S3.GetBucketMetricsConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
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
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations>
-- and
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources>.
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
module Amazonka.S3.GetBucketMetricsConfiguration
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newGetBucketMetricsConfiguration' smart constructor.
data GetBucketMetricsConfiguration = GetBucketMetricsConfiguration'
  { -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the bucket containing the metrics configuration to retrieve.
    bucket :: BucketName,
    -- | The ID used to identify the metrics configuration.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBucketMetricsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'getBucketMetricsConfiguration_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'bucket', 'getBucketMetricsConfiguration_bucket' - The name of the bucket containing the metrics configuration to retrieve.
--
-- 'id', 'getBucketMetricsConfiguration_id' - The ID used to identify the metrics configuration.
newGetBucketMetricsConfiguration ::
  -- | 'bucket'
  BucketName ->
  -- | 'id'
  Prelude.Text ->
  GetBucketMetricsConfiguration
newGetBucketMetricsConfiguration pBucket_ pId_ =
  GetBucketMetricsConfiguration'
    { expectedBucketOwner =
        Prelude.Nothing,
      bucket = pBucket_,
      id = pId_
    }

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
getBucketMetricsConfiguration_expectedBucketOwner :: Lens.Lens' GetBucketMetricsConfiguration (Prelude.Maybe Prelude.Text)
getBucketMetricsConfiguration_expectedBucketOwner = Lens.lens (\GetBucketMetricsConfiguration' {expectedBucketOwner} -> expectedBucketOwner) (\s@GetBucketMetricsConfiguration' {} a -> s {expectedBucketOwner = a} :: GetBucketMetricsConfiguration)

-- | The name of the bucket containing the metrics configuration to retrieve.
getBucketMetricsConfiguration_bucket :: Lens.Lens' GetBucketMetricsConfiguration BucketName
getBucketMetricsConfiguration_bucket = Lens.lens (\GetBucketMetricsConfiguration' {bucket} -> bucket) (\s@GetBucketMetricsConfiguration' {} a -> s {bucket = a} :: GetBucketMetricsConfiguration)

-- | The ID used to identify the metrics configuration.
getBucketMetricsConfiguration_id :: Lens.Lens' GetBucketMetricsConfiguration Prelude.Text
getBucketMetricsConfiguration_id = Lens.lens (\GetBucketMetricsConfiguration' {id} -> id) (\s@GetBucketMetricsConfiguration' {} a -> s {id = a} :: GetBucketMetricsConfiguration)

instance
  Core.AWSRequest
    GetBucketMetricsConfiguration
  where
  type
    AWSResponse GetBucketMetricsConfiguration =
      GetBucketMetricsConfigurationResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetBucketMetricsConfigurationResponse'
            Prelude.<$> (Data.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetBucketMetricsConfiguration
  where
  hashWithSalt _salt GetBucketMetricsConfiguration' {..} =
    _salt `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` id

instance Prelude.NFData GetBucketMetricsConfiguration where
  rnf GetBucketMetricsConfiguration' {..} =
    Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf id

instance Data.ToHeaders GetBucketMetricsConfiguration where
  toHeaders GetBucketMetricsConfiguration' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner
      ]

instance Data.ToPath GetBucketMetricsConfiguration where
  toPath GetBucketMetricsConfiguration' {..} =
    Prelude.mconcat ["/", Data.toBS bucket]

instance Data.ToQuery GetBucketMetricsConfiguration where
  toQuery GetBucketMetricsConfiguration' {..} =
    Prelude.mconcat ["id" Data.=: id, "metrics"]

-- | /See:/ 'newGetBucketMetricsConfigurationResponse' smart constructor.
data GetBucketMetricsConfigurationResponse = GetBucketMetricsConfigurationResponse'
  { -- | Specifies the metrics configuration.
    metricsConfiguration :: Prelude.Maybe MetricsConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetBucketMetricsConfigurationResponse
newGetBucketMetricsConfigurationResponse pHttpStatus_ =
  GetBucketMetricsConfigurationResponse'
    { metricsConfiguration =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Specifies the metrics configuration.
getBucketMetricsConfigurationResponse_metricsConfiguration :: Lens.Lens' GetBucketMetricsConfigurationResponse (Prelude.Maybe MetricsConfiguration)
getBucketMetricsConfigurationResponse_metricsConfiguration = Lens.lens (\GetBucketMetricsConfigurationResponse' {metricsConfiguration} -> metricsConfiguration) (\s@GetBucketMetricsConfigurationResponse' {} a -> s {metricsConfiguration = a} :: GetBucketMetricsConfigurationResponse)

-- | The response's http status code.
getBucketMetricsConfigurationResponse_httpStatus :: Lens.Lens' GetBucketMetricsConfigurationResponse Prelude.Int
getBucketMetricsConfigurationResponse_httpStatus = Lens.lens (\GetBucketMetricsConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetBucketMetricsConfigurationResponse' {} a -> s {httpStatus = a} :: GetBucketMetricsConfigurationResponse)

instance
  Prelude.NFData
    GetBucketMetricsConfigurationResponse
  where
  rnf GetBucketMetricsConfigurationResponse' {..} =
    Prelude.rnf metricsConfiguration
      `Prelude.seq` Prelude.rnf httpStatus
