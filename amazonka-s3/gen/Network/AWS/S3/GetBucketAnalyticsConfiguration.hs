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
-- Module      : Network.AWS.S3.GetBucketAnalyticsConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This implementation of the GET operation returns an analytics
-- configuration (identified by the analytics configuration ID) from the
-- bucket.
--
-- To use this operation, you must have permissions to perform the
-- @s3:GetAnalyticsConfiguration@ action. The bucket owner has this
-- permission by default. The bucket owner can grant this permission to
-- others. For more information about permissions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations>
-- and
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- For information about Amazon S3 analytics feature, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/analytics-storage-class.html Amazon S3 Analytics – Storage Class Analysis>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- __Related Resources__
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketAnalyticsConfiguration.html DeleteBucketAnalyticsConfiguration>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketAnalyticsConfigurations.html ListBucketAnalyticsConfigurations>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketAnalyticsConfiguration.html PutBucketAnalyticsConfiguration>
module Network.AWS.S3.GetBucketAnalyticsConfiguration
  ( -- * Creating a Request
    GetBucketAnalyticsConfiguration (..),
    newGetBucketAnalyticsConfiguration,

    -- * Request Lenses
    getBucketAnalyticsConfiguration_expectedBucketOwner,
    getBucketAnalyticsConfiguration_bucket,
    getBucketAnalyticsConfiguration_id,

    -- * Destructuring the Response
    GetBucketAnalyticsConfigurationResponse (..),
    newGetBucketAnalyticsConfigurationResponse,

    -- * Response Lenses
    getBucketAnalyticsConfigurationResponse_analyticsConfiguration,
    getBucketAnalyticsConfigurationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newGetBucketAnalyticsConfiguration' smart constructor.
data GetBucketAnalyticsConfiguration = GetBucketAnalyticsConfiguration'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the bucket from which an analytics configuration is
    -- retrieved.
    bucket :: BucketName,
    -- | The ID that identifies the analytics configuration.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBucketAnalyticsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'getBucketAnalyticsConfiguration_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'bucket', 'getBucketAnalyticsConfiguration_bucket' - The name of the bucket from which an analytics configuration is
-- retrieved.
--
-- 'id', 'getBucketAnalyticsConfiguration_id' - The ID that identifies the analytics configuration.
newGetBucketAnalyticsConfiguration ::
  -- | 'bucket'
  BucketName ->
  -- | 'id'
  Prelude.Text ->
  GetBucketAnalyticsConfiguration
newGetBucketAnalyticsConfiguration pBucket_ pId_ =
  GetBucketAnalyticsConfiguration'
    { expectedBucketOwner =
        Prelude.Nothing,
      bucket = pBucket_,
      id = pId_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
getBucketAnalyticsConfiguration_expectedBucketOwner :: Lens.Lens' GetBucketAnalyticsConfiguration (Prelude.Maybe Prelude.Text)
getBucketAnalyticsConfiguration_expectedBucketOwner = Lens.lens (\GetBucketAnalyticsConfiguration' {expectedBucketOwner} -> expectedBucketOwner) (\s@GetBucketAnalyticsConfiguration' {} a -> s {expectedBucketOwner = a} :: GetBucketAnalyticsConfiguration)

-- | The name of the bucket from which an analytics configuration is
-- retrieved.
getBucketAnalyticsConfiguration_bucket :: Lens.Lens' GetBucketAnalyticsConfiguration BucketName
getBucketAnalyticsConfiguration_bucket = Lens.lens (\GetBucketAnalyticsConfiguration' {bucket} -> bucket) (\s@GetBucketAnalyticsConfiguration' {} a -> s {bucket = a} :: GetBucketAnalyticsConfiguration)

-- | The ID that identifies the analytics configuration.
getBucketAnalyticsConfiguration_id :: Lens.Lens' GetBucketAnalyticsConfiguration Prelude.Text
getBucketAnalyticsConfiguration_id = Lens.lens (\GetBucketAnalyticsConfiguration' {id} -> id) (\s@GetBucketAnalyticsConfiguration' {} a -> s {id = a} :: GetBucketAnalyticsConfiguration)

instance
  Core.AWSRequest
    GetBucketAnalyticsConfiguration
  where
  type
    AWSResponse GetBucketAnalyticsConfiguration =
      GetBucketAnalyticsConfigurationResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetBucketAnalyticsConfigurationResponse'
            Prelude.<$> (Core.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetBucketAnalyticsConfiguration

instance
  Prelude.NFData
    GetBucketAnalyticsConfiguration

instance
  Core.ToHeaders
    GetBucketAnalyticsConfiguration
  where
  toHeaders GetBucketAnalyticsConfiguration' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner
      ]

instance Core.ToPath GetBucketAnalyticsConfiguration where
  toPath GetBucketAnalyticsConfiguration' {..} =
    Prelude.mconcat ["/", Core.toBS bucket]

instance Core.ToQuery GetBucketAnalyticsConfiguration where
  toQuery GetBucketAnalyticsConfiguration' {..} =
    Prelude.mconcat ["id" Core.=: id, "analytics"]

-- | /See:/ 'newGetBucketAnalyticsConfigurationResponse' smart constructor.
data GetBucketAnalyticsConfigurationResponse = GetBucketAnalyticsConfigurationResponse'
  { -- | The configuration and any analyses for the analytics filter.
    analyticsConfiguration :: Prelude.Maybe AnalyticsConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBucketAnalyticsConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analyticsConfiguration', 'getBucketAnalyticsConfigurationResponse_analyticsConfiguration' - The configuration and any analyses for the analytics filter.
--
-- 'httpStatus', 'getBucketAnalyticsConfigurationResponse_httpStatus' - The response's http status code.
newGetBucketAnalyticsConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBucketAnalyticsConfigurationResponse
newGetBucketAnalyticsConfigurationResponse
  pHttpStatus_ =
    GetBucketAnalyticsConfigurationResponse'
      { analyticsConfiguration =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The configuration and any analyses for the analytics filter.
getBucketAnalyticsConfigurationResponse_analyticsConfiguration :: Lens.Lens' GetBucketAnalyticsConfigurationResponse (Prelude.Maybe AnalyticsConfiguration)
getBucketAnalyticsConfigurationResponse_analyticsConfiguration = Lens.lens (\GetBucketAnalyticsConfigurationResponse' {analyticsConfiguration} -> analyticsConfiguration) (\s@GetBucketAnalyticsConfigurationResponse' {} a -> s {analyticsConfiguration = a} :: GetBucketAnalyticsConfigurationResponse)

-- | The response's http status code.
getBucketAnalyticsConfigurationResponse_httpStatus :: Lens.Lens' GetBucketAnalyticsConfigurationResponse Prelude.Int
getBucketAnalyticsConfigurationResponse_httpStatus = Lens.lens (\GetBucketAnalyticsConfigurationResponse' {httpStatus} -> httpStatus) (\s@GetBucketAnalyticsConfigurationResponse' {} a -> s {httpStatus = a} :: GetBucketAnalyticsConfigurationResponse)

instance
  Prelude.NFData
    GetBucketAnalyticsConfigurationResponse
