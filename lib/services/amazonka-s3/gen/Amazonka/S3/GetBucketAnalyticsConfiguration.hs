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
-- Module      : Amazonka.S3.GetBucketAnalyticsConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This implementation of the GET action returns an analytics configuration
-- (identified by the analytics configuration ID) from the bucket.
--
-- To use this operation, you must have permissions to perform the
-- @s3:GetAnalyticsConfiguration@ action. The bucket owner has this
-- permission by default. The bucket owner can grant this permission to
-- others. For more information about permissions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations>
-- and
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources>
-- in the /Amazon S3 User Guide/.
--
-- For information about Amazon S3 analytics feature, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/analytics-storage-class.html Amazon S3 Analytics – Storage Class Analysis>
-- in the /Amazon S3 User Guide/.
--
-- __Related Resources__
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketAnalyticsConfiguration.html DeleteBucketAnalyticsConfiguration>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketAnalyticsConfigurations.html ListBucketAnalyticsConfigurations>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketAnalyticsConfiguration.html PutBucketAnalyticsConfiguration>
module Amazonka.S3.GetBucketAnalyticsConfiguration
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newGetBucketAnalyticsConfiguration' smart constructor.
data GetBucketAnalyticsConfiguration = GetBucketAnalyticsConfiguration'
  { -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
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
-- 'expectedBucketOwner', 'getBucketAnalyticsConfiguration_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
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

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
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
  request overrides =
    Request.s3vhost
      Prelude.. Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetBucketAnalyticsConfigurationResponse'
            Prelude.<$> (Data.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetBucketAnalyticsConfiguration
  where
  hashWithSalt
    _salt
    GetBucketAnalyticsConfiguration' {..} =
      _salt `Prelude.hashWithSalt` expectedBucketOwner
        `Prelude.hashWithSalt` bucket
        `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    GetBucketAnalyticsConfiguration
  where
  rnf GetBucketAnalyticsConfiguration' {..} =
    Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf id

instance
  Data.ToHeaders
    GetBucketAnalyticsConfiguration
  where
  toHeaders GetBucketAnalyticsConfiguration' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner
      ]

instance Data.ToPath GetBucketAnalyticsConfiguration where
  toPath GetBucketAnalyticsConfiguration' {..} =
    Prelude.mconcat ["/", Data.toBS bucket]

instance Data.ToQuery GetBucketAnalyticsConfiguration where
  toQuery GetBucketAnalyticsConfiguration' {..} =
    Prelude.mconcat ["id" Data.=: id, "analytics"]

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
  where
  rnf GetBucketAnalyticsConfigurationResponse' {..} =
    Prelude.rnf analyticsConfiguration
      `Prelude.seq` Prelude.rnf httpStatus
