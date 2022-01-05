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
-- Module      : Amazonka.S3.DeleteBucketMetricsConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a metrics configuration for the Amazon CloudWatch request
-- metrics (specified by the metrics configuration ID) from the bucket.
-- Note that this doesn\'t include the daily storage metrics.
--
-- To use this operation, you must have permissions to perform the
-- @s3:PutMetricsConfiguration@ action. The bucket owner has this
-- permission by default. The bucket owner can grant this permission to
-- others. For more information about permissions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations>
-- and
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources>.
--
-- For information about CloudWatch request metrics for Amazon S3, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/cloudwatch-monitoring.html Monitoring Metrics with Amazon CloudWatch>.
--
-- The following operations are related to
-- @DeleteBucketMetricsConfiguration@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketMetricsConfiguration.html GetBucketMetricsConfiguration>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketMetricsConfiguration.html PutBucketMetricsConfiguration>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_ListBucketMetricsConfigurations.html ListBucketMetricsConfigurations>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/dev/cloudwatch-monitoring.html Monitoring Metrics with Amazon CloudWatch>
module Amazonka.S3.DeleteBucketMetricsConfiguration
  ( -- * Creating a Request
    DeleteBucketMetricsConfiguration (..),
    newDeleteBucketMetricsConfiguration,

    -- * Request Lenses
    deleteBucketMetricsConfiguration_expectedBucketOwner,
    deleteBucketMetricsConfiguration_bucket,
    deleteBucketMetricsConfiguration_id,

    -- * Destructuring the Response
    DeleteBucketMetricsConfigurationResponse (..),
    newDeleteBucketMetricsConfigurationResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newDeleteBucketMetricsConfiguration' smart constructor.
data DeleteBucketMetricsConfiguration = DeleteBucketMetricsConfiguration'
  { -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the bucket containing the metrics configuration to delete.
    bucket :: BucketName,
    -- | The ID used to identify the metrics configuration.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBucketMetricsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'deleteBucketMetricsConfiguration_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'bucket', 'deleteBucketMetricsConfiguration_bucket' - The name of the bucket containing the metrics configuration to delete.
--
-- 'id', 'deleteBucketMetricsConfiguration_id' - The ID used to identify the metrics configuration.
newDeleteBucketMetricsConfiguration ::
  -- | 'bucket'
  BucketName ->
  -- | 'id'
  Prelude.Text ->
  DeleteBucketMetricsConfiguration
newDeleteBucketMetricsConfiguration pBucket_ pId_ =
  DeleteBucketMetricsConfiguration'
    { expectedBucketOwner =
        Prelude.Nothing,
      bucket = pBucket_,
      id = pId_
    }

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
deleteBucketMetricsConfiguration_expectedBucketOwner :: Lens.Lens' DeleteBucketMetricsConfiguration (Prelude.Maybe Prelude.Text)
deleteBucketMetricsConfiguration_expectedBucketOwner = Lens.lens (\DeleteBucketMetricsConfiguration' {expectedBucketOwner} -> expectedBucketOwner) (\s@DeleteBucketMetricsConfiguration' {} a -> s {expectedBucketOwner = a} :: DeleteBucketMetricsConfiguration)

-- | The name of the bucket containing the metrics configuration to delete.
deleteBucketMetricsConfiguration_bucket :: Lens.Lens' DeleteBucketMetricsConfiguration BucketName
deleteBucketMetricsConfiguration_bucket = Lens.lens (\DeleteBucketMetricsConfiguration' {bucket} -> bucket) (\s@DeleteBucketMetricsConfiguration' {} a -> s {bucket = a} :: DeleteBucketMetricsConfiguration)

-- | The ID used to identify the metrics configuration.
deleteBucketMetricsConfiguration_id :: Lens.Lens' DeleteBucketMetricsConfiguration Prelude.Text
deleteBucketMetricsConfiguration_id = Lens.lens (\DeleteBucketMetricsConfiguration' {id} -> id) (\s@DeleteBucketMetricsConfiguration' {} a -> s {id = a} :: DeleteBucketMetricsConfiguration)

instance
  Core.AWSRequest
    DeleteBucketMetricsConfiguration
  where
  type
    AWSResponse DeleteBucketMetricsConfiguration =
      DeleteBucketMetricsConfigurationResponse
  request =
    Request.s3vhost
      Prelude.. Request.delete defaultService
  response =
    Response.receiveNull
      DeleteBucketMetricsConfigurationResponse'

instance
  Prelude.Hashable
    DeleteBucketMetricsConfiguration
  where
  hashWithSalt
    _salt
    DeleteBucketMetricsConfiguration' {..} =
      _salt `Prelude.hashWithSalt` expectedBucketOwner
        `Prelude.hashWithSalt` bucket
        `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    DeleteBucketMetricsConfiguration
  where
  rnf DeleteBucketMetricsConfiguration' {..} =
    Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf id

instance
  Core.ToHeaders
    DeleteBucketMetricsConfiguration
  where
  toHeaders DeleteBucketMetricsConfiguration' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner
      ]

instance Core.ToPath DeleteBucketMetricsConfiguration where
  toPath DeleteBucketMetricsConfiguration' {..} =
    Prelude.mconcat ["/", Core.toBS bucket]

instance
  Core.ToQuery
    DeleteBucketMetricsConfiguration
  where
  toQuery DeleteBucketMetricsConfiguration' {..} =
    Prelude.mconcat ["id" Core.=: id, "metrics"]

-- | /See:/ 'newDeleteBucketMetricsConfigurationResponse' smart constructor.
data DeleteBucketMetricsConfigurationResponse = DeleteBucketMetricsConfigurationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBucketMetricsConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteBucketMetricsConfigurationResponse ::
  DeleteBucketMetricsConfigurationResponse
newDeleteBucketMetricsConfigurationResponse =
  DeleteBucketMetricsConfigurationResponse'

instance
  Prelude.NFData
    DeleteBucketMetricsConfigurationResponse
  where
  rnf _ = ()
